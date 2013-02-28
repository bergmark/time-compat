{-# LANGUAGE RecordWildCards #-}

-- | Compatibility with the
--   <http://hackage.haskell.org/package/old-time old-time> package for the
--   \"new\" <http://hackage.haskell.org/package/time time> package.
--
--   This is useful for writing portable code; in particular, if you're
--   using the <http://hackage.haskell.org/package/directory directory>
--   package and you want your code to build with both GHC 7.6 and earlier
--   versions.  The version of @directory@ used with GHC 7.6 changed
--   a dependency from @old-time@ to @time@ which means its
--   @getModificationTime@ function now returns a 'Time.UTCTime' instead of
--   a 'OldTime.ClockTime'.  This type affects the public API of many
--   libraries that use it.  To make such libraries portable, port your
--   code to use the @time@ package and to only rely on 'Time.UTCTime' in
--   its public API, and call 'toUTCTime' on the values returned by
--   functions like @getModificationTime@, for example:
--
--     >fmap toUTCTime getModificationTime
--
--   If you're using @directory-1.2@, 'toUTCTime' will just be 'id' and the
--   original value is returned intact.  If you're using an older
--   @directory@, for example because you're building with GHC 7.4, the
--   'OldTime.ClockTime' returned by @getModificationTime@ will be
--   converted to a 'Time.UTCTime' and will be compatible with your code
--   ported to the new @time@ package.
module Data.Time.Compat
    ( -- * Converting between representations
      ToUTCTime(..)
    )
  where

import qualified Data.Time   as Time
import qualified System.Time as OldTime

class ToUTCTime a where
    toUTCTime :: a -> Time.UTCTime

instance ToUTCTime Time.UTCTime where
    toUTCTime = id

instance ToUTCTime OldTime.ClockTime where
    toUTCTime = toUTCTime . OldTime.toUTCTime

instance ToUTCTime OldTime.CalendarTime where
    toUTCTime OldTime.CalendarTime{..} =
        Time.UTCTime day diffTime
      where
        year = fromIntegral ctYear
        month = fromEnum ctMonth + 1
        day = Time.fromGregorian year month ctDay
        sec = ctHour*60^2 + ctMin*60 + ctSec
        pico = fromIntegral sec*10^12 + ctPicosec
        diffTime = Time.picosecondsToDiffTime pico
