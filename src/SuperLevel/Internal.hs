{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
module SuperLevel.Internal
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

import SuperLevel.Internal.MultiTableSpec
import SuperLevel.Internal.TableSpec
import SuperLevel.Internal.Types
