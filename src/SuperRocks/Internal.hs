{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
module SuperRocks.Internal
    ( Connection, withConnection
    , ReadResult(..)
    , TableSpec(..)
    , putTable, getTable
    , tableBatch, TableBatchOp(..)
    , tableScan, TableScan(..), TableScanDir(..)
    , MultiTableSpec(..)
    , MultiTableHandle(..), getMultiTableHandle
    , multiTablePut
    , multiTablePutMany
    , multiTableScan, MultiTableScan(..)
    )
where

import SuperRocks.Internal.MultiTableSpec
import SuperRocks.Internal.TableSpec
import SuperRocks.Internal.Types
