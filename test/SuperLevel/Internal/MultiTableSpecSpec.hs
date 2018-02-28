{-# LANGUAGE OverloadedStrings #-}
module SuperLevel.Internal.MultiTableSpecSpec (spec) where

import SuperLevel.Codec.Serialise
import SuperLevel.Internal.MultiTableSpec
import SuperLevel.Internal.TableSpec
import SuperLevel.Internal.Types
import Control.Monad

import Data.Monoid
import System.IO.Temp
import Test.Hspec

testTable :: MultiTableSpec Int Int
testTable =
    MultiTableSpec
    TableSpec
    { t_namespace = "test"
    , t_keyRepr = serialiseRepr
    , t_valRepr = serialiseRepr
    }

withTestConn :: (Connection -> IO a) -> IO a
withTestConn go =
    withSystemTempDirectory "databaseTest" $ \dir ->
    withConnection (dir ++ "/db.db") go

startAtKey :: Int -> MultiTableScan Int
startAtKey i = MultiTableScan (Just i) TsFwd

spec :: Spec
spec =
    do it "reads a single written multi key value" $
           withTestConn $ \conn ->
           do hdl <- getMultiTableHandle conn testTable
              multiTablePut conn hdl 42 100
              res <-
                  multiTableScan conn hdl (startAtKey 42) $ \loadNext ->
                  do let loop accum =
                             do x <- loadNext
                                case x of
                                  RNotFound -> pure (reverse accum)
                                  RValue (_, v) -> loop (v : accum)
                                  RDecodeError errMsg -> fail (show errMsg)
                     loop []
              res `shouldBe` [100]
       it "reads many written multi key value" $
           withTestConn $ \conn ->
           do hdl <- getMultiTableHandle conn testTable
              multiTablePutMany conn hdl $ map (\x -> (42, x)) [1..100]
              res <-
                  multiTableScan conn hdl (startAtKey 42) $ \loadNext ->
                  do let loop accum =
                             do x <- loadNext
                                case x of
                                  RNotFound -> pure (reverse accum)
                                  RValue (_, v) -> loop (v : accum)
                                  RDecodeError errMsg -> fail (show errMsg)
                     loop []
              res `shouldBe` [1..100]
       it "reads many written multi key value w/o getting distracted by other pairs" $
           withTestConn $ \conn ->
           do hdl <- getMultiTableHandle conn testTable
              multiTablePutMany conn hdl $
                  map (\x -> (41, x)) [1..100] <>
                  map (\x -> (42, x)) [1..100] <>
                  map (\x -> (43, x)) [1..100]
              res <-
                  multiTableScan conn hdl (startAtKey 42) $ \loadNext ->
                  do let loop accum =
                             do x <- loadNext
                                case x of
                                  RNotFound -> pure (reverse accum)
                                  RValue (k, v) | k == 42 -> loop (v : accum)
                                  RValue _ -> pure (reverse accum)
                                  RDecodeError errMsg -> fail (show errMsg)
                     loop []
              res `shouldBe` [1..100]
       it "reads many written multi key value with closing and opening again (different keys)" $
           withSystemTempDirectory "databaseTest" $ \fp ->
           do let fname = fp ++ "/db.db"
              forM_ [0..50] $ \k ->
                  withConnection fname $ \conn ->
                  do hdl <- getMultiTableHandle conn testTable
                     multiTablePutMany conn hdl $ map (\x -> (k, x)) [1..100]
                     res <-
                         multiTableScan conn hdl (startAtKey k) $ \loadNext ->
                         do let loop accum =
                                    do x <- loadNext
                                       case x of
                                         RNotFound -> pure (reverse accum)
                                         RValue (_, v) -> loop (v : accum)
                                         RDecodeError errMsg -> fail (show errMsg)
                            loop []
                     res `shouldBe` [1..100]
       it "reads many written multi key value with closing and opening again (same key)" $
           withSystemTempDirectory "databaseTest" $ \fp ->
           do let fname = fp ++ "/db.db"
              forM_ [0..50] $ \i ->
                  withConnection fname $ \conn ->
                  do hdl <- getMultiTableHandle conn testTable
                     multiTablePutMany conn hdl $
                         map (\x -> (42, x)) [(100 * i)..(100 * (i + 1) - 1)]
                     res <-
                         multiTableScan conn hdl (startAtKey 42) $ \loadNext ->
                         do let loop accum =
                                    do x <- loadNext
                                       case x of
                                         RNotFound -> pure (reverse accum)
                                         RValue (_, v) -> loop (v : accum)
                                         RDecodeError errMsg -> fail (show errMsg)
                            loop []
                     res `shouldBe` [0..(100* (i+1) - 1)]
