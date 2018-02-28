{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module SuperRocks.TimeSeriesSpec (spec) where

import SuperRocks.Codec.Serialise
import SuperRocks.Internal.Types
import SuperRocks.TimeSeries

import Data.Time.Clock.POSIX
import System.IO.Temp
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic

testTable1 :: TimeSeriesTable Int
testTable1 =
    TimeSeriesTable
    { tst_namespace = "test"
    , tst_valRepr = serialiseRepr
    }

withTestConn :: TimeSeriesTable v -> (TimeSeriesHandle v -> IO a) -> IO a
withTestConn tst go =
    withSystemTempDirectory "databaseTest" $ \dir ->
    withConnection (dir ++ "/db.db") $ \conn ->
    do hdl <- getTimeSeriesHandle conn tst
       go hdl

spec :: Spec
spec =
    do prop "reads written values" $ \(Positive i :: Positive Int) v ->
           monadicIO $
           do let entry = TimeSeriesEntry (posixSecondsToUTCTime $ fromIntegral i) v
              res <-
                  run $ withTestConn testTable1 $ \hdl ->
                  do writeSingleEvent hdl entry
                     timeSeriesScan hdl (TimeSeriesScan Nothing TsFwd) id
              assert (RValue entry == res)
       it "supports batch writing" $
           withTestConn testTable1 $ \hdl ->
           do let batchVals :: [(Int, Int)]
                  batchVals =
                      map (\k -> (k, k * 2)) [1..100]
                  batch =
                      map (\(i, v) -> TimeSeriesEntry (posixSecondsToUTCTime $ fromIntegral i) v)
                      batchVals
              withTimeSeriesWriter hdl $ \write ->
                  mapM_ write batch
              let scan =
                      TimeSeriesScan
                      { tss_start = Nothing
                      , tss_scanDir = TsFwd
                      }
              r <-
                  timeSeriesScan hdl scan $ \getMore ->
                  do let loop accum =
                             do x <- getMore
                                case x of
                                  RNotFound -> pure (reverse accum)
                                  RValue v -> loop (v : accum)
                                  RDecodeError errMsg -> fail (show errMsg)
                     loop []
              r `shouldBe` batch
