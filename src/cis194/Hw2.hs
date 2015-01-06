{-#OPTIONS_GHC -Wall -Werror #-}
module Hw2 where

import Hw2Log
import Test.HUnit

--- Exercise 1

parseMessage :: String -> LogMessage
parseMessage s = case words s of ("E":i:t:m) -> LogMessage (Error (read i)) (read t) (unwords m)
                                 ("I":t:m)   -> LogMessage Info (read t) (unwords m)
                                 ("W":t:m)   -> LogMessage Warning (read t) (unwords m)
                                 _           -> Unknown s

parse :: String -> [LogMessage]
parse = map parseMessage . lines

runParseMessageTests :: IO Counts
runParseMessageTests = runTestTT allTests
  where
    allTests = TestList $ map baseTest testCases
    baseTest (input, result) = TestCase $ assertEqual "" result (parseMessage input)
    testCases = [ ("I 6 Completed armadillo processing", LogMessage Info 6 "Completed armadillo processing")
                , ("W 5 Flange is due for a check-up",   LogMessage Warning 5 "Flange is due for a check-up")
                , ("E 20 2 Too many pickles",            LogMessage (Error 20) 2 "Too many pickles")
                , ("abcdef",                             Unknown "abcdef") ]


--- Exercise 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree                                         = tree
insert lm Leaf                                                  = Node Leaf lm Leaf
insert lm@(LogMessage _ ts _) (Node lt (LogMessage _ tts _) rt) | ts <= tts = insert lm lt
                                                                | ts > tts  = insert lm rt
insert _ _                                                      = undefined

-- runInsertTests = runTestTT insertTests
--   where
--     insertTests = TestList $ map baseTest testCases
--     baseTest (input1, input2) result = TestCase $ assertEquals "" result (insert input1 input2)
--     testCases = [ (LogMessage]


lmg :: Int -> LogMessage
lmg n = LogMessage Info n ""

sampleTree :: MessageTree
sampleTree = Node
              (Node
                Leaf
                (lmg 10)
                Leaf)
              (lmg 20)
              (Node
                Leaf
                (lmg 30)
                Leaf)
