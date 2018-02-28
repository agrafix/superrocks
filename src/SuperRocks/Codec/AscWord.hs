module SuperRocks.Codec.AscWord
    ( ascWordRepr )
where

import SuperRocks.Codec.Serialise
import SuperRocks.Codec.Types

import Data.Monoid
import Data.Word
import qualified Data.ByteString as BS

padSize :: Int
padSize = 10

padKey :: BS.ByteString -> BS.ByteString
padKey ptr =
    if BS.length ptr < padSize
    then BS.replicate (padSize - BS.length ptr) 0 <> ptr
    else ptr

ascWordRepr :: BinRepr Word64
ascWordRepr =
    BinRepr
    { br_write = padKey . br_write serialiseRepr
    , br_read =
            \bs ->
                if BS.all (== 0) bs
                then Right 0
                else br_read serialiseRepr $ BS.dropWhile (== 0) bs
    }
