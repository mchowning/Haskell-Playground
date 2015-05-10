{-#OPTIONS_GHC -Wall -Werror #-}

import Data.List
import Test.HUnit

type Month = String
type Day = String
data Date = Date { month :: Month, day :: Day } deriving (Eq)
-- data Month = Month String deriving Show
-- data Day = Day String deriving Show

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


-- findAnswer :: [Date]
-- findAnswer = let a = dates
--                  b = dates
--                  c = dates
--              in (nub (map month dates))

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

-- ################
-- #2
-- ################

-- Remove all months that have a unique date
  -- find all unique "days" of the month
  -- find all dates containing a month that has no unique "days" (i.e., 18 and 19)
  -- remove any dates with a month matching those "days" (i.e., remove any month that has a possible birthday on 18 or 19)


filterFor2 :: [Date]
filterFor2 = let allMonths = nub . map month $ dates
                 monthsWithoutUniqueDays = filter (\m -> any (\d -> hasMonthWithDay m d dates) getUniqueDays) allMonths
                 datesWithoutUniqueDays = filter (\d -> month d `elem` monthsWithoutUniqueDays) dates
             in datesWithoutUniqueDays

hasMonthWithDay :: Month -> Day -> [Date] -> Bool
hasMonthWithDay m d = any (flip dateHasDay d) . flip onlyKeepMonth m

getUniqueDays :: [Day]
getUniqueDays = map day $ filter (flip isUniqueDay dates . day) dates

isUniqueDay :: Day -> [Date] -> Bool
isUniqueDay d = (== 1) . length . filter (== d) . map day

dateHasDay :: Date -> Day -> Bool
dateHasDay = (==) . day

tests2 :: [Test]
tests2 = [
        "hasMonthWithDay: true" ~: hasMonthWithDay "May" "13" [Date "May" "13"] ~?= True
        , "hasMonthWithDay: matching day in diff month" ~: hasMonthWithDay "June" "13" [Date "May" "13"] ~?= False
        , "hasMonthWithDay: matching day in diff day" ~: hasMonthWithDay "May" "14" [Date "May" "13"] ~?= False
        ]

