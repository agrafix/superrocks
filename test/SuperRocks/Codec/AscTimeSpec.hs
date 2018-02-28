{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module SuperRocks.Codec.AscTimeSpec (spec) where

import SuperRocks.Codec.AscTime
import SuperRocks.Codec.Types

import Data.Time
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

newtype WrappedTime =
    WrappedTime { _unWrappedTime :: UTCTime }
    deriving (Show, Eq, Ord)

instance Arbitrary WrappedTime where
    arbitrary =
        do secs :: Double <- choose (0, 86400)
           let truncated :: Rational
               truncated =
                   let i :: Int
                       i = truncate (secs * 1000 * 1000)
                   in fromIntegral i / (1000 * 1000)
           (day :: Integer)  <- choose (120 * 365, 400 * 365)
           pure $ WrappedTime $ UTCTime (ModifiedJulianDay day) (fromRational truncated)

spec :: Spec
spec =
    do prop "encoder matches decoder" $ \(WrappedTime w) ->
           br_read ascTimeRepr (br_write ascTimeRepr w) == Right w
       prop "if w <= v then enc(w) <= enc(v)" $ \(WrappedTime w) (WrappedTime v) ->
           w <= v ==> br_write ascTimeRepr w <= br_write ascTimeRepr v
