{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module SuperLevel.Internal.TableSpecSpec (spec) where

import SuperLevel.Codec.Serialise
import SuperLevel.Internal.TableSpec
import SuperLevel.Internal.Types

import Data.List
import System.IO.Temp
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Monadic
import qualified Data.Set as S

testTable1 :: TableSpec Int Int
testTable1 =
    TableSpec
    { t_namespace = "test"
    , t_keyRepr = serialiseRepr
    , t_valRepr = serialiseRepr
    }


testTable2 :: TableSpec Int Int
testTable2 =
    TableSpec
    { t_namespace = "test2"
    , t_keyRepr = serialiseRepr
    , t_valRepr = serialiseRepr
    }

withTestConn :: (Connection -> IO a) -> IO a
withTestConn go =
    withSystemTempDirectory "databaseTest" $ \dir ->
    withConnection (dir ++ "/db.db") go

spec :: Spec
spec =
    do prop "reads written values" $ \k v ->
           monadicIO $
           do res <-
                  run $ withTestConn $ \conn ->
                  do putTable conn testTable1 k v
                     getTable conn testTable1 k
              assert (RValue v == res)
       prop "streams written values" $ \(S.toList -> ks) ->
           monadicIO $
           do resVals <-
                  run $ withTestConn $ \conn ->
                  do tableBatch conn testTable1 $
                         flip map ks $ \k -> TboInsert k (k * 2)
                     let scan =
                             TableScan
                             { ts_start = Nothing
                             , ts_scanDir = TsFwd
                             }
                     tableScan conn testTable1 scan $ \getMore ->
                         do let loop accum =
                                    do x <- getMore
                                       case x of
                                         RNotFound -> pure (reverse accum)
                                         RValue v -> loop (v : accum)
                                         RDecodeError errMsg -> fail (show errMsg)
                            loop []
              assert (sort resVals == map (\x -> (x, x*2)) (sort ks))
       prop "no table mixup" $ \tbl k v ->
           monadicIO $
           do res <-
                  run $ withTestConn $ \conn ->
                  do putTable conn (if tbl then testTable1 else testTable2) k v
                     getTable conn (if tbl then testTable1 else testTable2) k
              assert (RValue v == res)
