{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
module SuperLevel.Codec.Types
    ( BinRepr(..), byteRepr
    , eitherToFail
    )
where

import Data.ByteString (ByteString)
import qualified Data.Text as T

data BinRepr v
    = BinRepr
    { br_write :: v -> ByteString
    , br_read :: ByteString -> Either T.Text v
    }

byteRepr :: BinRepr ByteString
byteRepr =
    BinRepr
    { br_write = id
    , br_read = Right
    }

eitherToFail :: Monad m => String -> (e -> String) -> Either e v -> m v
eitherToFail info showError x =
    case x of
      Left errMsg -> fail (info ++ ": " ++ showError errMsg)
      Right ok -> pure ok
