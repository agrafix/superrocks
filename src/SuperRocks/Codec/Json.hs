module SuperRocks.Codec.Json
    ( jsonRepr, jsonCborRepr )
where

import SuperRocks.Codec.Types

import Codec.CBOR.JSON
import Codec.CBOR.Read
import Codec.CBOR.Write
import Data.Aeson
import Data.Bifunctor
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

jsonRepr :: (ToJSON s, FromJSON s) => BinRepr s
jsonRepr =
    BinRepr
    { br_write = BSL.toStrict . encode
    , br_read = first T.pack . eitherDecodeStrict'
    }

jsonCborRepr :: (ToJSON s, FromJSON s) => BinRepr s
jsonCborRepr =
    BinRepr
    { br_write = toStrictByteString . encodeValue . toJSON
    , br_read =
            \bsl ->
            case deserialiseFromBytes (decodeValue False) (BSL.fromStrict bsl) of
              Left errMsg -> Left (T.pack $ show errMsg)
              Right (_, val) ->
                  case fromJSON val of
                    Error errMsg -> Left (T.pack errMsg)
                    Success ok -> Right ok
    }
