module Age.Parser
  (
    days
  , weeks
  , months
  , years
  ) where

import           Age.Dates           (addAgeUnit, daysBetween, monthsBetween,
                                      weeksBetween, yearsBetween)
import           Age.Types           (AgeUnit (..), Day, Range)
import           Control.Monad.State
import           Data.Semigroup      (Semigroup, (<>))

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
days = AgeParser $ \(s, e) ->
  let ds = daysBetween s e
  in (pure ds, (addAgeUnit ds s, e))

weeks :: AgeParser
weeks = AgeParser $ \(s, e) ->
  let ws = weeksBetween s e
  in (pure ws, (addAgeUnit ws s, e))

months :: AgeParser
months = AgeParser $ \(s, e) ->
  let ms = monthsBetween s e
  in (pure ms, (addAgeUnit ms s, e))

years :: AgeParser
years = AgeParser $ \(s, e) ->
  let ys = yearsBetween s e
  in (pure ys, (addAgeUnit ys s, e))


ageParser :: (Day -> Day -> AgeUnit) -> [AgeUnit] -> State Range [AgeUnit]
ageParser f as = do
  (s, e) <- get
  let a = f s e
  put (addAgeUnit a s, e)
  return (a : as)

days' :: [AgeUnit] -> State Range [AgeUnit]
days' = ageParser daysBetween

weeks' = ageParser weeksBetween
months' = ageParser monthsBetween
years' = ageParser yearsBetween
