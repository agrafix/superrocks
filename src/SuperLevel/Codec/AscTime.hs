module SuperLevel.Codec.AscTime
    ( ascTimeRepr )
where

import SuperLevel.Codec.AscWord
import SuperLevel.Codec.Types

import Data.Bifunctor
import Data.Time
import Data.Time.Clock.POSIX
import Data.Word

timeToWord64 :: UTCTime -> Word64
timeToWord64 t =
    let x = utcTimeToPOSIXSeconds t * 1000 * 1000
        w = truncate x
    in w

word64ToTime :: Word64 -> UTCTime
word64ToTime x =
    posixSecondsToUTCTime $
    fromIntegral x / (1000 * 1000)

ascTimeRepr :: BinRepr UTCTime
ascTimeRepr =
    BinRepr
    { br_write = br_write ascWordRepr . timeToWord64
    , br_read = second word64ToTime . br_read ascWordRepr
    }
