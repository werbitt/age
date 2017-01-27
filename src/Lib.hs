module Lib
    (
    ) where

import           Age.NaturalLanguage
import           Age.Parser          (AgeParser, age, days, mkAgeParser, months,
                                      parseAge, weeks, years)
import           Age.Types           (Age)
import           Data.Semigroup      ((<>))
import           Data.Time           (Day, fromGregorian)
import           Data.Time.Clock     (UTCTime (..), getCurrentTime)


ageNow :: AgeParser -> Day -> IO Age
ageNow p s = do
  (UTCTime now _) <- getCurrentTime
  return $ age (parseAge p (s, now))

test = ageNow (days <> weeks <> months <> years) (fromGregorian 1982 5 8)
