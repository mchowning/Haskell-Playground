{-#OPTIONS_GHC -Wall -Werror #-}

-------------------------------------------------------------------------------------------------
-- Cheryl's birthday puzzle from: http://nbviewer.ipython.org/url/norvig.com/ipython/Cheryl.ipynb
-------------------------------------------------------------------------------------------------

import Data.List
import Test.HUnit

type Month = String
type Day = String
data Date = Date { month :: Month, day :: Day } deriving (Eq)

instance Show Date where
        show (Date m d) = m ++ ' ':d

dates :: [Date]
dates = [ Date "May" "15"
        , Date "May" "16"
        , Date "May" "19"
        , Date "June" "17"
        , Date "June" "18"
        , Date "July" "14"
        , Date "July" "16"
        , Date "August" "14"
        , Date "August" "15"
        , Date "August" "17"]

runTests :: IO Counts
runTests = runTestTT . TestList $
        tests1
        ++ tests2

----------------------------------------------------------------------------------------------

onlyKeepMonth :: [Date] -> Month -> [Date]
onlyKeepMonth ds m = filter ((== m) . month) ds

onlyKeepDay :: [Date] -> Day -> [Date]
onlyKeepDay ds d = filter ((== d) . day) ds

tests1 :: [Test]
tests1 = [
        "onlyKeepMonth 1" ~: onlyKeepMonth [Date "May" "13"] "June" ~?= []
        , "onlyKeepMonth 2" ~: onlyKeepMonth [Date "May" "13"] "May" ~?= [Date "May" "13"]
        , "onlyKeepDay 1" ~: onlyKeepDay [Date "May" "13"] "14" ~?= []
        , "onlyKeepDay 1" ~: onlyKeepDay [Date "May" "13"] "13" ~?= [Date "May" "13"]
        ]

---------------------------------------------------------------------------------------------
-- #3
-- Albert: I don't know when Cheryl's birthday is, but I know that Bernard does not know too.

-- Remove all months that have a unique date
  -- find all unique "days" of the month
  -- find all dates containing a month that has no unique "days" (i.e., 18 and 19)
  -- remove any dates with a month matching those "days" (i.e., remove any month that has a possible birthday on 18 or 19)


datesFromMonthsWithoutUniqueDays :: [Date]
datesFromMonthsWithoutUniqueDays = filter (flip elem monthsWithoutUniqueDays . month) dates

allMonths :: [Month]
allMonths = nub. map month $ dates

monthsWithoutUniqueDays :: [Month]
monthsWithoutUniqueDays = filter (not . flip elem monthsWithUniqueDays) allMonths

monthsWithUniqueDays :: [Month]
monthsWithUniqueDays = filter (flip (hasMonthWithOneOfDays dates) allUniqueDays) allMonths

hasMonthWithOneOfDays :: [Date] -> Month -> [Day] -> Bool
hasMonthWithOneOfDays mds m = any (hasDateWithMonthAndDay mds m)

hasDateWithMonthAndDay :: [Date] -> Month -> Day -> Bool
hasDateWithMonthAndDay ds m d = Date m d `elem` ds
-- hasDateWithMonthAndDay ds = (flip elem) ds . Date
-- FIXME

allUniqueDays :: [Day]
allUniqueDays = map day $ filter (flip isUniqueDay dates . day) dates
-- uniqueDays = map day $ filter (isUniqueDay dates . day) dates

isUniqueDay :: Day -> [Date] -> Bool
isUniqueDay d = (== 1) . length . filter ((== d) . day)
-- FIXME why do i have to have the parentheses around the == d?
-- isUniqueDay :: [Date] -> Day -> Bool

tests2 :: [Test]
tests2 = [
        "hasMonthWithDay: true" ~: hasDateWithMonthAndDay [Date "May" "13"] "May" "13" ~?= True
        , "hasMonthWithDay: matching day in diff month" ~: hasDateWithMonthAndDay [Date "May" "13"] "June" "13" ~?= False
        , "hasMonthWithDay: matching day in diff day" ~: hasDateWithMonthAndDay [Date "May" "13"] "May" "14" ~?= False
        ]

---------------------------------------------------------------------------------------------
-- #4
-- Bernard: At first I don't know when Cheryl's birthday is, but I know now.

-- input is datesFromMonthsWithoutUniqueDays

-- remove any dates that are in both months

datesWithUniqueDays :: [Date]
datesWithUniqueDays = filter (flip isUniqueDay datesFromMonthsWithoutUniqueDays . day) datesFromMonthsWithoutUniqueDays

---------------------------------------------------------------------------------------------
-- #5
-- Albert: Then I also know when Cheryl's birthday is.

-- input is datesWithUniqueDays

result :: [Date]
result = filter ((== 1) . numberTimesMonthInList . month) datesWithUniqueDays

numberTimesMonthInList :: Month -> Int
numberTimesMonthInList m = length . filter ((== m) . month) $ datesWithUniqueDays
