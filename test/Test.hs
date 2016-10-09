import           Data.Semigroup                 ((<>))
import           Data.Time                      (Day (..), fromGregorian,
                                                 toGregorian)
import           Data.Time.Calendar.MonthDay    (monthLength)
import           Data.Time.Calendar.OrdinalDate (isLeapYear)
import           Lib
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck          as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [qcProps, unitTests]

instance Arbitrary Day where
  arbitrary = pure .  ModifiedJulianDay =<< arbitrary

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ semigroupProps
  , monthProps
  , yearProps]


-- prop_days_and_weeks_are_isomorphic :: TestTree
-- prop_days_and_weeks_are_isomorphic
--   = QC.testProperty "days == weeks * 7 + remainder" $
--     \s e -> days s e == fst (weeks s e) * 7 + snd (weeks s e)
semigroupProps :: TestTree
semigroupProps = testGroup "<> works"
  [ QC.testProperty "weeks <> days works as expected"
  prop_mappend_for_weeks_and_days
  ]


prop_mappend_for_weeks_and_days :: (Day, Day) -> Property
prop_mappend_for_weeks_and_days dts
  = parseAge (days <> weeks) dts ===
    parseAge (AgeParser $ \r ->
                            let (w, r') = parseAge weeks r
                                (d, r'') = parseAge days r'
                            in (d ++ w, r'')) dts

monthProps :: TestTree
monthProps = testGroup "months"
  [ QC.testProperty
    "Zero months between dates in same month"
    prop_zero_months_within_month
  , QC.testProperty
    "Zero or one month between dates in consecutive months"
    prop_consecutive_months
  ]

yearProps :: TestTree
yearProps = testGroup "years"
  [ QC.testProperty
    "Zero years between dates in same year"
    prop_zero_years_within_year
  ]

arbitraryDatesInSameMonth :: Gen (Day, Day)
arbitraryDatesInSameMonth = do
  (y, m, _)  <- toGregorian <$> arbitrary
  let ml = monthLength (isLeapYear y) m
  d <- choose (1, ml)
  d' <- choose (d, ml)
  return (fromGregorian y m d, fromGregorian y m d')

prop_zero_months_within_month :: Property
prop_zero_months_within_month =  forAll arbitraryDatesInSameMonth $
                                 \ds -> (fst . parseAge months) ds == pure (Months 0)

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




unitTests :: TestTree
unitTests = testGroup "(checked by HUnit)"
  []

testMonths :: TestTree
testMonths = testGroup "months"
  [ testCase "Same month" undefined ]
