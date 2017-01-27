{-# LANGUAGE OverloadedStrings #-}

module Age.NaturalLanguage
  (
    label
  , ageUnitToEnglish
  , ageToEnglish
  , joinWithOxford
  ) where

import           Age.Types   (Age, AgeUnit (..), extract)
import           Data.List   (sortBy)
import           Data.Maybe  (mapMaybe)
import           Data.Monoid ((<>))
import           Data.Ord    (Down (..), comparing)
import           Data.Text   (Text, pack)


label :: AgeUnit -> Text
label (Days   _) = "day"
label (Weeks  _) = "week"
label (Months _) = "month"
label (Years  _) = "year"

ageUnitToEnglish :: AgeUnit -> Maybe Text
ageUnitToEnglish a = let n = extract a
                     in case n of
                          0 -> Nothing
                          1 -> Just $ "1 " <> label a
                          _ -> Just $ pack (show n) <> " " <> label a <> "s"

ageToEnglish :: Age -> Maybe Text
ageToEnglish a = let ts =  mapMaybe ageUnitToEnglish $ sortBy (comparing Down) a
                 in case ts of
                      []        -> Nothing
                      [t,t'] -> Just $ t <> " and " <> t'
                      _         -> Just $ joinWithOxford ts

joinWithOxford :: [Text] -> Text
joinWithOxford [] = ""
joinWithOxford [x,y] = x <> ", and " <> y
joinWithOxford (x:xs) = x <>  ", " <> joinWithOxford xs
