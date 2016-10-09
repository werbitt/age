module Age.Types
  (
    Range
  , AgeUnit (..)
  , extract
  , Day
  ) where

import           Data.Time (Day)


type Range = (Day, Day)

data AgeUnit = Days Integer
             | Weeks Integer
             | Months Integer
             | Years Integer deriving (Eq, Show)

extract :: AgeUnit -> Integer
extract (Days   n) = n
extract (Weeks  n) = n
extract (Months n) = n
extract (Years  n) = n
