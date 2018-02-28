{-# LANGUAGE OverloadedStrings #-}
module SuperRocks.Codec.AscWordSpec (spec) where

import SuperRocks.Codec.AscWord
import SuperRocks.Codec.Types


import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec =
    do prop "encoder matches decoder" $ \w ->
           br_read ascWordRepr (br_write ascWordRepr w) == Right w
       prop "if w <= v then enc(w) <= enc(v)" $ \w v ->
           w <= v ==> br_write ascWordRepr w <= br_write ascWordRepr v
