module Age.Dates
  (
    addMonthsNextValid
  , fromGregorianNextValid
  , daysBetween
  ) where

import           Age.Types  (AgeUnit (..))
import           Data.Maybe (fromMaybe)
import           Data.Time  (Day, diffDays, fromGregorian, fromGregorianValid,
                             toGregorian)

daysBetween :: Day -> Day -> AgeUnit
daysBetween s e = Days $ diffDays e s

weeksBetween :: Day -> Day -> AgeUnit
weeksBetween = undefined

addMonthsNextValid :: Integer -> Day -> Day
addMonthsNextValid n dt = fromGregorianNextValid (y + ys + ry) (fromInteger m') d
  where
    (y, m, d) = toGregorian dt
    (ys, ms) = divMod n 12
    -- | Take the remaining months and add them to the month of the original
    -- date. If this is greater than 12 we'll roll over one more year, and the
    -- remainder will become our new start month
    (ry, m') = divMod (ms + toInteger m) 12

-- | Like fromGregorian except if given an invalid day, it increments
-- the month and sets the day to 1
fromGregorianNextValid :: Integer -> Int -> Int -> Day
fromGregorianNextValid y m d
  = fromMaybe (case m of
                 12 -> fromGregorian (y+1) 1 1
                 _  -> fromGregorian y (m + 1) 1)
    (fromGregorianValid y m d)
