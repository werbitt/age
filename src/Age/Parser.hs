module Age.Parser
  (
  ) where

import           Age.Dates      (addMonthsNextValid, daysBetween,
                                 fromGregorianNextValid)
import           Age.Types      (AgeUnit (..), Day, Range, extract)
import           Data.Semigroup (Semigroup, (<>))
import           Data.Time      (addDays)

newtype AgeParser = AgeParser (Range -> ([AgeUnit], Range))

type AgeParserResult = ([AgeUnit], Range)

parseAge :: AgeParser -> (Day, Day) ->  AgeParserResult
parseAge (AgeParser f) = f

instance Semigroup AgeParser where
  a <> b
    = AgeParser $ \r ->
                 let (x, r') = parseAge b r
                     (x', r'') = parseAge a r'
                 in (x' ++ x, r'')

days :: AgeParser
days = AgeParser $ \(s, e) -> (pure $ daysBetween s e, (e, e))

weeks :: AgeParser
weeks = AgeParser $ \(s, e) ->
  let ws = div (extract $ daysBetween s e) 7
      s' = addDays (ws * 7) s
  in (pure (Weeks ws), (s', e))

months :: AgeParser
months = AgeParser $ \(s, e) ->
  let (y1, m1, d1) = toGregorian s
      (y2, m2, d2) = toGregorian e
      ms = ((y2 - y1) * 12) + toInteger (m2 - m1)
      ms' = if d2 < d1 then max 0 (ms - 1) else ms
      s' = addMonthsNextValid ms' s
  in (pure $ Months ms', (s', e))


-- | Returns the number of years between a start 'day' and an end 'day' as well as the
-- ''Range' that is that many years from the start 'day'. This 'day' can be used to calculate
-- the additional days, weeks, or months between the days
years :: AgeParser
years = AgeParser $ \(s, e) ->
  let (y1, m1, d1) = toGregorian s
      (y2, m2, d2) = toGregorian e
      ys = y2 - y1
      ys' = case compare m2 m1 of
        LT -> max 0 (ys-1)
        EQ -> if d2 < d1 then max 0 (ys-1) else ys
        GT -> ys
      s' = fromGregorianNextValid (y1 + ys') m1 d1
  in (pure $ Years ys', (s', e))
