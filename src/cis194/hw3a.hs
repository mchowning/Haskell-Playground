-- {-#OPTIONS_GHC -Wall -Werror #-}

import Test.HUnit

--- Exercise 1

skips :: [a] -> [[a]]
skips ls =
  map (everyNOther ls) [1..(length ls)]
  where
    everyNOther :: [a] -> Int -> [a]
    everyNOther ls' n = map snd (filter (isMultiple n . fst) (zip [1..] ls'))

    -- TODO why can't I define the type if I just use the original ls variable here instead of passing it?
--     everyNOther :: Int -> [a]
--     everyNOther n = map snd (filter ((isMultiple n) . fst) (zip [1..] ls))

    isMultiple :: Int -> Int -> Bool
    isMultiple n = (== 0) . (`mod` n)


runSkipsTests :: IO Counts
runSkipsTests = runTestTT allTests
  where
    allTests = TestList
      [ ["ABCD", "BD", "C", "D"]               ~=? skips "ABCD"
      , ["hello!", "el!", "l!", "l", "o", "!"] ~=? skips "hello!"
      , ["a"]                                  ~=? skips "a"
      , ([]::[[Bool]])                         ~=? skips ([]::[Bool])
      -- TODO what is going on here?
--       , ([[]]::[[Bool]])                       ~=? skips ([]::[Bool])
--       , [""]                                   ~=? skips ""
      ]
