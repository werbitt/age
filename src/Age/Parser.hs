module Age.Parser
  ( AgeParser(..)
  , AgeParserResult
  , parseAge
  , mkAgeParser
  , days
  , weeks
  , months
  , years
  ) where

import           Age.Dates      (addAgeUnit, daysBetween, monthsBetween,
                                 weeksBetween, yearsBetween)
import           Age.Types      (AgeUnit, Day, Range)
import           Data.Semigroup

newtype AgeParser = AgeParser (Range -> ([AgeUnit], Range))

type AgeParserResult = ([AgeUnit], Range)

parseAge :: AgeParser -> Range ->  AgeParserResult
parseAge (AgeParser f) = f

instance Semigroup AgeParser where
  a <> b
    = AgeParser $ \r ->
                 let (x, r') = parseAge b r
                     (x', r'') = parseAge a r'
                 in (x' ++ x, r'')

mkAgeParser :: (Day -> Day -> AgeUnit) -> AgeParser
mkAgeParser f = AgeParser $ \(s, e) ->
  let n = f s e
  in (pure n, (addAgeUnit n s, e))

days :: AgeParser
days = mkAgeParser daysBetween

weeks :: AgeParser
weeks = mkAgeParser weeksBetween

months :: AgeParser
months = mkAgeParser monthsBetween

years :: AgeParser
years = mkAgeParser yearsBetween
