module Age.Dates
  (
    addMonthsNextValid
  , fromGregorianNextValid
  , daysBetween
  , weeksBetween
  , monthsBetween
  , yearsBetween
  , addAgeUnit
  ) where

import           Age.Types  (AgeUnit (..), extract)
import           Data.Maybe (fromMaybe)
import           Data.Time  (Day, addDays, diffDays, fromGregorian,
                             fromGregorianValid, toGregorian)

daysBetween :: Day -> Day -> AgeUnit
daysBetween s e = Days $ diffDays e s

weeksBetween :: Day -> Day -> AgeUnit
weeksBetween s e = Weeks $ div (extract $ daysBetween s e) 7

monthsBetween :: Day -> Day -> AgeUnit
monthsBetween dt dt' = let (y , m , d ) = toGregorian dt
                           (y', m', d') = toGregorian dt'
                           ms = ((y' - y) * 12) + toInteger (m' - m)
                           ms' = if d' < d then max 0 (ms - 1) else ms
                       in Months ms'

yearsBetween :: Day -> Day -> AgeUnit
yearsBetween dt dt' = let (y , m , d ) = toGregorian dt
                          (y', m', d') = toGregorian dt'
                          ys = y' - y
                          ys' = case compare m' m of
                            LT -> max 0 (ys - 1)
                            GT -> ys
                            EQ -> if d' < d then max 0 (ys - 1) else ys
                      in Years ys'


addAgeUnit :: AgeUnit -> Day -> Day
addAgeUnit (Days n) dt = addDays n dt
addAgeUnit (Weeks n) dt = addDays (n * 7) dt
addAgeUnit (Months n) dt = addMonthsNextValid n dt
addAgeUnit (Years n) dt = let (y, m, d) = toGregorian dt
                          in fromGregorianNextValid (y + n) m d


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
