module Lib
    (
      ageNow
    , age
    , ageInWords
    , ageNowInWords
    ) where

import           Age.NaturalLanguage (ageToEnglish)
import           Age.Parser          (defaultParser, evalAge)
import           Age.Types           (Age)
import           Data.Text           (Text)
import           Data.Time           (Day)
import           Data.Time.Clock     (UTCTime (..), getCurrentTime)


ageNow :: Day -> IO Age
ageNow b = do
  (UTCTime now _) <- getCurrentTime
  return $ evalAge defaultParser (b, now)

age :: Day -> Day -> Age
age = curry (evalAge defaultParser)

ageInWords :: Day -> Day -> Maybe Text
ageInWords = (ageToEnglish .) .  age

ageNowInWords :: Day -> IO (Maybe Text)
ageNowInWords = (ageToEnglish <$>) .  ageNow
