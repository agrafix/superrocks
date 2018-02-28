{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
module SuperLevel.Internal.TableSpec
    ( TableSpec(..)
    , tableBatch, TableBatchOp(..)
    , putTable, getTable
    , tableScan, TableScan(..), TableScanDir(..)
    )
where

import SuperLevel.Codec.Types
import SuperLevel.Internal.Types

import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)
import Data.Monoid
import qualified Data.ByteString as BS
import qualified Database.RocksDB as DB

data TableSpec k v
    = TableSpec
    { t_namespace :: ByteString
    , t_keyRepr :: BinRepr k
    , t_valRepr :: BinRepr v
    }

data TableBatchOp k v
    = TboInsert k v
    | TboDelete k

tableKeySpace :: TableSpec k v -> ByteString
tableKeySpace TableSpec{..} =
    t_namespace <> ".values"

tableKey :: TableSpec k v -> k -> ByteString
tableKey spec@TableSpec{..} k =
    tableKeySpace spec
    <> "."
    <> br_write t_keyRepr k

tableBatch :: MonadIO m => Connection -> TableSpec k v -> [TableBatchOp k v] -> m ()
tableBatch (Connection conn) spec@TableSpec{..} ops =
    DB.write conn DB.defaultWriteOptions $
    flip map ops $ \op ->
    case op of
      TboInsert k v ->
          DB.Put (tableKey spec k) (br_write t_valRepr v)
      TboDelete k ->
          DB.Del (tableKey spec k)

putTable :: MonadIO m => Connection -> TableSpec k v -> k -> v -> m ()
putTable (Connection conn) spec@TableSpec{..} k v =
    DB.put conn DB.defaultWriteOptions (tableKey spec k) (br_write t_valRepr v)

getTable :: MonadIO m => Connection -> TableSpec k v -> k -> m (ReadResult v)
getTable (Connection conn) spec@TableSpec{..} k =
    do mBs <- DB.get conn DB.defaultReadOptions (tableKey spec k)
       case mBs of
         Nothing -> pure RNotFound
         Just bs ->
             case br_read t_valRepr bs of
               Left err -> pure $ RDecodeError err
               Right ok -> pure $ RValue ok

data TableScanDir
    = TsFwd
    | TsRev

data TableScan k
    = TableScan
    { ts_start :: Maybe k
    , ts_scanDir :: TableScanDir
    }

tableScan ::
    (MonadBaseControl IO m, MonadIO m, MonadThrow m)
    => Connection
    -> TableSpec k v
    -> TableScan k
    -> (m (ReadResult (k, v)) -> m a)
    -> m a
tableScan (Connection conn) spec@TableSpec{..} TableScan{..} run =
    runResourceT $ DB.withIterator conn DB.defaultReadOptions $ \it ->
    do let keySpace = tableKeySpace spec
           keySpaceSize = BS.length keySpace
           inKeySpace b = BS.take keySpaceSize b == keySpace
       case ts_start of
         Just startKey -> DB.iterSeek it (tableKey spec startKey)
         Nothing -> DB.iterSeek it keySpace
       let handleValue keyBs valueBs =
               do let parsedKey =
                          br_read t_keyRepr $
                          BS.drop (keySpaceSize + 1) keyBs
                      parsedValue =
                          br_read t_valRepr valueBs
                  case (parsedKey, parsedValue) of
                    (Right k, Right v) -> pure (RValue (k, v))
                    (Left err, _) -> pure (RDecodeError err)
                    (_, Left err) -> pure (RDecodeError err)
           action =
               do currentKey <- DB.iterKey it
                  case currentKey of
                    Nothing -> pure RNotFound
                    Just ck
                        | inKeySpace ck ->
                              do currentVal <- DB.iterValue it
                                 case currentVal of
                                   Nothing -> pure RNotFound
                                   Just cBs ->
                                       do case ts_scanDir of
                                            TsFwd -> DB.iterNext it
                                            TsRev -> DB.iterPrev it
                                          handleValue ck cBs
                        | otherwise -> pure RNotFound

       lift $ run action
