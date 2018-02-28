{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}
module SuperRocks.Internal.Types
    ( Connection(..)
    , withConnection
    , ReadResult(..)
    )
where

import Control.Exception
import qualified Data.Text as T
import qualified Database.RocksDB as DB

newtype Connection
    = Connection { _unConnection :: DB.DB }

withConnection :: FilePath -> (Connection -> IO a) -> IO a
withConnection fp go =
    bracket (DB.open fp cfg) DB.close $ \db ->
    go (Connection db)
    where
      cfg =
          DB.defaultOptions
          { DB.createIfMissing = True
          }

data ReadResult v
    = RDecodeError T.Text
    | RNotFound
    | RValue v
    deriving (Show, Eq, Functor)
