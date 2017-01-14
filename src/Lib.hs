module Lib
    (
    ) where

import           Age.NaturalLanguage
import           Age.Parser          (AgeParser, days, evalAgeParser,
                                      mkAgeParser, months, weeks, years)
import qualified Age.ParserNoState   as P
import           Age.Types           (Age)
import           Data.Semigroup      ((<>))
import           Data.Time           (Day, fromGregorian)
import           Data.Time.Clock     (UTCTime (..), getCurrentTime)

ageNow :: AgeParser -> Day -> IO Age
ageNow p s = do
  (UTCTime now _) <- getCurrentTime
  return $ evalAgeParser p (s, now)

age :: AgeParser -> Day -> Day -> Age
age p s e = evalAgeParser p (s, e)



test = ageNow (mkAgeParser years >>= weeks >>= days) (fromGregorian 1982 5 8)

ageNow' :: P.AgeParser -> Day -> IO Age
ageNow' p s = do
  (UTCTime now _) <- getCurrentTime
  return $ fst (P.parseAge p (s, now))

test' = ageNow' (P.days <> P.weeks <> P.months <> P.years) (fromGregorian 1982 5 8)
