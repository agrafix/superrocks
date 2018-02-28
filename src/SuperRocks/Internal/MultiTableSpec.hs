{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
module SuperRocks.Internal.MultiTableSpec
    ( MultiTableSpec(..)
    , MultiTableHandle(..), getMultiTableHandle
    , multiTablePut
    , multiTablePutMany
    , multiTableScan, MultiTableScan(..)
    )
where

import SuperRocks.Codec.AscWord
import SuperRocks.Codec.Types
import SuperRocks.Internal.TableSpec
import SuperRocks.Internal.Types

import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)
import Data.IORef
import Data.Monoid
import Data.Word
import qualified Data.ByteString as BS

data MultiTableSpec k v
    = MultiTableSpec
    { mts_table :: TableSpec k v
    }

data MultiTableHandle k v
    = MultiTableHandle
    { mth_spec :: MultiTableSpec k v
    , mth_ptr :: IORef Word64
      -- ^ can store > 300 years of data at 20000 records per seconds
    }

innerTableSpec :: MultiTableSpec k v -> TableSpec ByteString ByteString
innerTableSpec MultiTableSpec{..} =
    TableSpec
    { t_namespace = t_namespace mts_table
    , t_keyRepr = byteRepr
    , t_valRepr = byteRepr
    }

ptrKey :: ByteString
ptrKey = "meta.ptr"

multiKeyValueSpace :: ByteString
multiKeyValueSpace = "values"

multiKeySpace :: MultiTableSpec k v -> k -> ByteString
multiKeySpace mts k =
    multiKeyValueSpace
    <> "."
    <> br_write (t_keyRepr (mts_table mts)) k

multiKey :: MultiTableSpec k v -> ByteString -> k -> ByteString
multiKey mts ptr k =
    multiKeySpace mts k
    <> "."
    <> ptr

getMultiTableHandle ::
    MonadIO m => Connection
    -> MultiTableSpec k v -> m (MultiTableHandle k v)
getMultiTableHandle c mts =
    do maxHandleBs <- getTable c (innerTableSpec mts) ptrKey
       maxHandle <-
           case maxHandleBs of
             RNotFound -> pure 0
             RDecodeError _ -> pure 0 -- should never happen as decode is noop!
             RValue bs ->
                 case br_read ascWordRepr bs of
                   Left errMsg -> fail ("Failed to read sequence counter. " ++ show errMsg)
                   Right ok -> pure ok
       ptr <- liftIO $ newIORef maxHandle
       pure MultiTableHandle { mth_spec = mts, mth_ptr = ptr }

multiTablePut :: MonadIO m => Connection -> MultiTableHandle k v -> k -> v -> m ()
multiTablePut conn mth k v = multiTablePutMany conn mth [(k, v)]

multiTablePutMany :: MonadIO m => Connection -> MultiTableHandle k v -> [(k, v)] -> m ()
multiTablePutMany c mth kvs =
    do let spec = mth_spec mth
           go [] !ptr !accum =
               pure (ptr, accum)
           go ((k, v) : xs) !_ !accum =
               do ptr <-
                      liftIO $
                      br_write ascWordRepr <$>
                      atomicModifyIORef' (mth_ptr mth) (\x -> (x + 1, x + 1))
                  let action =
                          TboInsert (multiKey spec ptr k) (br_write (t_valRepr $ mts_table spec) v)
                  go xs ptr (action : accum)
       (finalPtr, writeActions) <- go kvs "" []
       case writeActions of
         [] -> pure ()
         _ ->
             tableBatch c (innerTableSpec $ mth_spec mth)
             ( TboInsert ptrKey finalPtr
             : writeActions
             )

data MultiTableScan k
    = MultiTableScan
    { mts_start :: Maybe k
    , mts_scanDir :: TableScanDir
    }

multiTableScan ::
    (MonadBaseControl IO m, MonadIO m, MonadThrow m)
    => Connection
    -> MultiTableHandle k v
    -> MultiTableScan k
    -> (m (ReadResult (k, v)) -> m a)
    -> m a
multiTableScan c mth scanCfg run =
    tableScan c (innerTableSpec $ mth_spec mth) scan $ \next ->
    run $ wrap next
    where
      keySpaceSize = BS.length multiKeyValueSpace
      inKeySpace b = BS.take keySpaceSize b == multiKeyValueSpace
      wrap next =
          do val <- next
             case val of
               RDecodeError err -> pure (RDecodeError err)
               RNotFound -> pure RNotFound
               RValue (innerK, rawVal)
                 | inKeySpace innerK ->
                       do let parsedKey =
                                  br_read (t_keyRepr (mts_table $ mth_spec mth)) $
                                  BS.drop (keySpaceSize + 1) innerK
                              parsedValue =
                                  br_read (t_valRepr (mts_table $ mth_spec mth)) rawVal
                          case (parsedKey, parsedValue) of
                            (Right k, Right v) -> pure (RValue (k, v))
                            (Left err, _) -> pure (RDecodeError err)
                            (_, Left err) -> pure (RDecodeError err)
                 | otherwise -> pure RNotFound
      scan =
          TableScan
          { ts_start =
                  Just $
                  case mts_start scanCfg of
                    Just startKey -> multiKeySpace (mth_spec mth) startKey
                    Nothing -> multiKeyValueSpace
          , ts_scanDir = mts_scanDir scanCfg
          }
