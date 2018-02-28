{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module SuperLevel.TimeSeries
    ( TimeSeriesTable(..)
    , TimeSeriesHandle
    , getTimeSeriesHandle
    , withTimeSeriesWriter, TimeSeriesEntry(..)
    , writeSingleEvent
    , timeSeriesScan, TimeSeriesScan(..), TableScanDir(..)
    )
where

import SuperLevel.Codec.AscTime
import SuperLevel.Codec.Types
import SuperLevel.Internal.MultiTableSpec
import SuperLevel.Internal.TableSpec
import SuperLevel.Internal.Types

import Control.Batch
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Time
import Data.Time.TimeSpan
import qualified Data.ByteString as BS

data TimeSeriesTable v
    = TimeSeriesTable
    { tst_namespace :: BS.ByteString
    , tst_valRepr :: BinRepr v
    }

data TimeSeriesHandle v
    = TimeSeriesHandle
    { tsh_conn :: Connection
    , tsh_table :: MultiTableHandle UTCTime v
    }

data TimeSeriesEntry v
    = TimeSeriesEntry
    { tse_time :: UTCTime
    , tse_value :: v
    } deriving (Show, Eq)

getTimeSeriesHandle ::
    MonadIO m => Connection -> TimeSeriesTable v -> m (TimeSeriesHandle v)
getTimeSeriesHandle c tst =
    do mh <- getMultiTableHandle c spec
       pure TimeSeriesHandle { tsh_conn = c, tsh_table = mh }
    where
      spec =
          MultiTableSpec
          TableSpec
          { t_namespace = tst_namespace tst
          , t_keyRepr = ascTimeRepr
          , t_valRepr = tst_valRepr tst
          }

writeSingleEvent :: MonadIO m => TimeSeriesHandle v -> TimeSeriesEntry v -> m ()
writeSingleEvent TimeSeriesHandle{..} tse =
    multiTablePut tsh_conn tsh_table (tse_time tse) (tse_value tse)

withTimeSeriesWriter ::
    ( MonadBaseControl IO m
    , MonadIO m
    ) => TimeSeriesHandle v -> ((TimeSeriesEntry v -> m ()) -> m a) -> m a
withTimeSeriesWriter TimeSeriesHandle{..} action =
    withBatchRunner batchCfg $ \hdl -> action (bh_enqueue hdl)
    where
      batchCfg =
          Batch
          { b_runEveryItems = Just 10
          , b_runAfterTimeout = Just (seconds 5)
          , b_maxQueueLength = Just 10000
          , b_runBatch = \els ->
                  multiTablePutMany tsh_conn tsh_table $ flip map els $ \e ->
                  (tse_time e, tse_value e)
          }

data TimeSeriesScan
    = TimeSeriesScan
    { tss_start :: Maybe UTCTime
    , tss_scanDir :: TableScanDir
    }

timeSeriesScan ::
    ( MonadBaseControl IO m
    , MonadIO m
    , MonadThrow m
    )
    => TimeSeriesHandle v
    -> TimeSeriesScan
    -> (m (ReadResult (TimeSeriesEntry v)) -> m a)
    -> m a
timeSeriesScan tsh tss run =
    multiTableScan (tsh_conn tsh) (tsh_table tsh) scanCfg $ \next ->
    run $ wrap next
    where
      wrap next =
          fmap (fmap $ uncurry TimeSeriesEntry) next
      scanCfg =
          MultiTableScan
          { mts_start = tss_start tss
          , mts_scanDir = tss_scanDir tss
          }
