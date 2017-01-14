module ParserNoStateTest
  (
    qcProps
  ) where

import           Age.ParserNoState
import           Age.Types
import           Data.Semigroup                 ((<>))
import           Data.Time                      (Day (..), fromGregorian,
                                                 toGregorian)
import           Data.Time.Calendar.MonthDay    (monthLength)
import           Data.Time.Calendar.OrdinalDate (isLeapYear)
import           Test.Tasty
import           Test.Tasty.QuickCheck

instance Arbitrary Day where
  arbitrary = pure .  ModifiedJulianDay =<< arbitrary

qcProps :: TestTree
qcProps = testGroup "ParserNoState Properties"
  [ semigroupProps
  , monthProps
  , yearProps]

-- prop_days_and_weeks_are_isomorphic :: TestTree
-- prop_days_and_weeks_are_isomorphic
--   = QC.testProperty "days == weeks * 7 + remainder" $
--     \s e -> days s e == fst (weeks s e) * 7 + snd (weeks s e)

semigroupProps :: TestTree
semigroupProps = testGroup "<> works"
  [ testProperty "weeks <> days works as expected"
  prop_mappend_for_weeks_and_days
  , testProperty "mappend is associative with AgeParsers"
  prop_associativity
  ]


prop_mappend_for_weeks_and_days :: (Day, Day) -> Property
prop_mappend_for_weeks_and_days dts
  = parseAge (days <> weeks) dts ===
    parseAge (AgeParser $ \r ->
                            let (w, r') = parseAge weeks r
                                (d, r'') = parseAge days r'
                            in (d ++ w, r'')) dts

prop_associativity :: (Day, Day) -> Property
prop_associativity r@(s, e) = e >= s ==>
  parseAge ((days <> weeks) <> months) r === parseAge (days <> (weeks <> months)) r

monthProps :: TestTree
monthProps = testGroup "months"
  [ testProperty
    "Zero months between dates in same month"
    prop_zero_months_within_month
  , testProperty
    "Zero months between dates in same month'"
    prop_zero_months_within_month
  , testProperty
    "Zero or one month between dates in consecutive months"
    prop_consecutive_months
  ]

yearProps :: TestTree
yearProps = testGroup "years"
  [ testProperty
    "Zero years between dates in same year"
    prop_zero_years_within_year
  ]

prop_zero_months_within_month :: (Day, Day) -> Property
prop_zero_months_within_month r@(s, e)
  =  e >= s &&  monthAndYear e == monthAndYear s ==>
     (fst . parseAge months) r === pure (Months 0)
  where
    monthAndYear dt = let (y, m, _) = toGregorian dt
                      in (m, y)



arbitraryDatesInConsecutiveMonths :: Gen (Day, Day)
arbitraryDatesInConsecutiveMonths = do
  (y, m, _) <- toGregorian <$> arbitrary
  let (y', m') = if m == 12
                 then (y + 1, 1)
                 else (y, m + 1)
      ml  = monthLength (isLeapYear y) m
      ml' = monthLength (isLeapYear y') m'
  d  <- choose (1, ml)
  d' <- choose (1, ml')
  return (fromGregorian y m d, fromGregorian y' m' d')

prop_consecutive_months :: Property
prop_consecutive_months = forAll arbitraryDatesInConsecutiveMonths $
  \dts@(dt, dt') -> let (_, _, d) = toGregorian dt
                        (_, _, d') = toGregorian dt'
                        ms = fst $ parseAge months dts
                    in if d > d' then ms === pure (Months 0) else ms === pure (Months 1)

prop_zero_years_within_year :: Property
prop_zero_years_within_year = forAll datesInSameYear $
  \dts -> (fst . parseAge years) dts == pure (Years 0)
  where
    datesInSameYear :: Gen (Day, Day)
    datesInSameYear = do
      dt <- arbitrary
      let (y, m , d) = toGregorian dt
      m' <- choose (m, 12)
      d' <- if m' == m
            then choose (d, monthLength (isLeapYear y) m')
            else choose (1, monthLength (isLeapYear y) m')
      return (dt, fromGregorian y m' d')
