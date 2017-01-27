module Age.Parser
  ( AgeParser(..)
  , parseAge
  , mkAgeParser
  , evalAge
  , days
  , weeks
  , months
  , years
  , defaultParser
  ) where

import           Age.Dates      (addAgeUnit, daysBetween, monthsBetween,
                                 weeksBetween, yearsBetween)
import           Age.Types      (Age, AgeUnit, Day, Range)
import           Data.Semigroup

type AgeParserResult = (Age, Range)
newtype AgeParser = AgeParser (Range -> AgeParserResult)

parseAge :: AgeParser -> Range ->  AgeParserResult
parseAge (AgeParser f) = f

evalAge :: AgeParser -> Range -> Age
evalAge (AgeParser f) = fst . f

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

defaultParser :: AgeParser
defaultParser = years <> months <> weeks <> days
