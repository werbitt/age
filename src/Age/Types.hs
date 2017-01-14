module Age.Types
  (
    Range
  , AgeUnit (..)
  , Age
  , extract
  , Day
  ) where

import           Data.Time (Day)


type Range = (Day, Day)

data AgeUnit = Days Integer
             | Weeks Integer
             | Months Integer
             | Years Integer deriving (Eq, Show, Ord)

type Age = [AgeUnit]

extract :: AgeUnit -> Integer
extract (Days   n) = n
extract (Weeks  n) = n
extract (Months n) = n
extract (Years  n) = n
