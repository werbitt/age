{-# LANGUAGE RankNTypes #-}
module Lib
    (
      days
    , weeks
    , years
    , months
    , extract
    , parseAge
    , AgeUnit (..)
    , AgeParser (..)
    ) where

import           Data.Maybe         (fromMaybe)
import           Data.Semigroup     (Semigroup, (<>))
import           Data.Time.Calendar (Day, addDays, diffDays, fromGregorian,
                                     fromGregorianValid, toGregorian)

-- | An age can be represented in different ways.
-- days is simply the number of days between start date and end date.
-- weeks is also simple, its the number of days divided by 7, and the remainder is days.
--
-- Once you start using months it gets tricky because different months have a different
-- number of days. The same is true for years, due to leap years. So there is no function
-- from age in days to age in years or months. Ages that are the same in years or months
-- can actually be different in days.
