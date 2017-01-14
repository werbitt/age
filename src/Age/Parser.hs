module Age.Parser
  (
    ageParser
  , days
  , weeks
  , months
  , years
  , evalAgeParser
  , AgeParser(..)
  , mkAgeParser
  ) where

import           Age.Dates           (addAgeUnit, daysBetween, monthsBetween,
                                      weeksBetween, yearsBetween)
import           Age.Types           (Age, AgeUnit (..), Day, Range)
import           Control.Monad.Loops (concatM)
import           Control.Monad.State

type AgeParser = State Range Age

ageParser :: (Day -> Day -> AgeUnit) -> Age -> AgeParser
ageParser f as = do
  (s, e) <- get
  let a = f s e
  put (addAgeUnit a s, e)
  return (a : as)

days :: Age -> AgeParser
days = ageParser daysBetween

weeks :: Age -> AgeParser
weeks = ageParser weeksBetween

months :: Age -> AgeParser
months = ageParser monthsBetween

years :: Age -> AgeParser
years = ageParser yearsBetween

evalAgeParsers :: [Age -> AgeParser] -> Range -> Age
evalAgeParsers = evalState . flip concatM []

evalAgeParser :: AgeParser -> Range -> Age
evalAgeParser = evalState

mkAgeParser :: (Age -> AgeParser) -> AgeParser
mkAgeParser f = f []
