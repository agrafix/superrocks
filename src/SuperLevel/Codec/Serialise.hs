module SuperLevel.Codec.Serialise
    ( serialiseRepr )
where

import SuperLevel.Codec.Types

import Codec.Serialise
import Data.Bifunctor
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

serialiseRepr :: Serialise s => BinRepr s
serialiseRepr =
    BinRepr
    { br_write = BSL.toStrict . serialise
    , br_read = first (T.pack . show) . deserialiseOrFail . BSL.fromStrict
    }
