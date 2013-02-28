{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Data.Time.Compat                     (toUTCTime)
import qualified Data.Time.Format                     as Time
import           System.Locale                        (defaultTimeLocale)
import qualified System.Time                          as OldTime
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.Framework.TH                    (defaultMainGenerator)
import           Test.QuickCheck                      (Property, (==>))

main :: IO ()
main = $defaultMainGenerator

-- TODO: more explicit format string and compare picos properly
-- TODO: test that timezones are handled correctly
prop_formatTime :: Integer -> Integer -> Property
prop_formatTime sec pico = pico >= 0 && pico <= 999999999999 ==>
    OldTime.formatCalendarTime defaultTimeLocale fmt old
      ==
    Time.formatTime defaultTimeLocale fmt new
  where
    fmt = "%c"
    old = OldTime.toUTCTime $ OldTime.TOD sec pico
    new = toUTCTime old
