{-#OPTIONS_GHC -Wall -Werror #-}
module Hw2_1 where

import Log
import Data.Maybe
import Text.Read
import Test.HUnit

--- Exercise 1


parseMessage :: String -> LogMessage
parseMessage s = case words s of
  ("E":errorLevel:timestamp:message) | isANumber errorLevel && isANumber timestamp
                                                -> generateErrorLogMessage errorLevel timestamp message
  ("I":timestamp:message) | isANumber timestamp -> generateLogMessage Info timestamp message
  ("W":timestamp:message) | isANumber timestamp -> generateLogMessage Warning timestamp message
  _                                             -> Unknown s
  where
    generateErrorLogMessage :: String -> String -> [String] -> LogMessage
    generateErrorLogMessage el = generateLogMessage (Error $ read el)

    generateLogMessage :: MessageType -> String -> [String] -> LogMessage
    generateLogMessage mt ts m = LogMessage mt (read ts) (unwords m)


parse :: String -> [LogMessage]
parse = map parseMessage . lines


isANumber :: String -> Bool
isANumber str = isJust (readMaybe str::Maybe Int)


runParseMessageTests :: IO Counts
runParseMessageTests = runTestTT allTests
  where
    allTests = TestList $ map baseTest testCases
    baseTest (input, expected) = expected ~=? parseMessage input
    testCases = [ ("I 6 Completed armadillo processing", LogMessage Info 6 "Completed armadillo processing")
                , ("W 5 Flange is due for a check-up"  , LogMessage Warning 5 "Flange is due for a check-up")
                , ("E 20 2 Too many pickles"           , LogMessage (Error 20) 2 "Too many pickles")
                , ("abcdef"                            , Unknown "abcdef")
                , ("W notanumber a a"                  , Unknown "W notanumber a a")
                , ("E notanumber 1111111111 a a"       , Unknown "E notanumber 1111111111 a a")
                , ("E 1111111111 notanumber a a"       , Unknown "E 1111111111 notanumber a a")
                ]


--- Exercise 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree                                                     = tree
insert lm Leaf                                                              = Node Leaf lm Leaf
insert lm@(LogMessage _ ts _) (Node left nodeLm@(LogMessage _ tts _) right) | ts <= tts = Node (insert lm left) nodeLm right
                                                                            | ts > tts  = Node left nodeLm (insert lm right)
insert _ _                                                                  = undefined

runInsertTests :: IO Counts
runInsertTests = runTestTT allTests
  where
    allTests :: Test
    allTests = TestList $ map baseTest testCases
    baseTest (input, expected) = expected ~=? uncurry insert input
    testCases = [ ((lmg 10, Leaf)         , bottomNode 10)                              -- Leaf insert
                , ((lmg 15, bottomNode 10), Node Leaf (lmg 10) (bottomNode 15))         -- Right insert
                , ((lmg 5, bottomNode 10) , Node (bottomNode 5) (lmg 10) Leaf) ]        -- Left insert

    bottomNode :: Int -> MessageTree
    bottomNode n = Node Leaf (lmg n) Leaf

lmg :: Int -> LogMessage
lmg n = LogMessage Info n ""


--- Exercise 3

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

runBuildTests :: IO Counts
runBuildTests = runTestTT allTests
  where
    allTests :: Test
    allTests = TestList $ map baseTest testCases
    baseTest (input, expected) = expected ~=? build input
    testCases :: [ ([LogMessage], MessageTree) ]
    testCases =  [ ([lmg 1],
                    Node Leaf (lmg 1) Leaf)                                               -- 1 element
                 , ([lmg 1, lmg 3, lmg 2],
                    Node (Node Leaf (lmg 1) Leaf) (lmg 2) (Node Leaf (lmg 3) Leaf))       -- 3 elements, evenly split
                 , ([lmg 1, lmg 2, lmg 3],
                    Node (Node (Node Leaf (lmg 1) Leaf) (lmg 2) Leaf) (lmg 3) Leaf)       -- 3 elements, all left side
                 , ([lmg 1, lmg 3, lmg 2],
                    Node (Node Leaf (lmg 1) Leaf) (lmg 2) (Node Leaf (lmg 3) Leaf)) ]     -- 3 elements, all right side


--- Exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lt m rt) = inOrder lt ++ [m] ++ inOrder rt

runInOrderTests :: IO Counts
runInOrderTests = runTestTT allTests
  where
    allTests = TestList $ map baseTest testCases
    baseTest (input, expected) = expected ~=? inOrder input
    testCases :: [ (MessageTree, [LogMessage]) ]
    testCases =  [ (Node Leaf (lmg 1) Leaf,                                              -- 1 element
                   [lmg 1])
                 , (Node (Node Leaf (lmg 1) Leaf) (lmg 2) (Node Leaf (lmg 3) Leaf),      -- 3 elements, evenly split
                   [lmg 1, lmg 2, lmg 3])
                 , (Node (Node (Node Leaf (lmg 1) Leaf) (lmg 2) Leaf) (lmg 3) Leaf,      -- 3 elements, all left side
                   [lmg 1, lmg 2, lmg 3])
                 , (Node (Node Leaf (lmg 1) Leaf) (lmg 2) (Node Leaf (lmg 3) Leaf),      -- 3 elements, all right side
                   [lmg 1, lmg 2, lmg 3]) ]


--- Exercise 5

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map messageGetter . filter onlySevereErrors . inOrder . build
  where
    onlySevereErrors :: LogMessage -> Bool
    onlySevereErrors (LogMessage (Error n) _ _) = n >= 50
    onlySevereErrors _                          = False

    messageGetter :: LogMessage -> String
    messageGetter (LogMessage _ _ m) = m
    messageGetter _                  = undefined

runWhatWentWrongTests :: IO Counts
runWhatWentWrongTests = runTestTT allTests
  where
    allTests = TestList $ map baseTest testCases
    baseTest (input, expected) = expected ~=? whatWentWrong input
    testCases :: [([LogMessage], [String])]
    testCases = [ ([LogMessage (Error 51) 1000 "error > 50"], ["error > 50"])
                , ([LogMessage (Error 49) 1000 "error < 50"], [])
                , ([LogMessage Info 1000 "info message"], [])
                ]

runWhatWentWrongTestSample :: IO Counts
runWhatWentWrongTestSample = do
                               strs <- testWhatWentWrong parse whatWentWrong "sample.log"
                               runTestTT (strs ~?= [ "Way too many pickles"
                                                 , "Bad pickle-flange interaction detected"
                                                 , "Flange failed!" ])