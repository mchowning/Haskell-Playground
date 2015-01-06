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
    baseTest (input, expected) = expected ~=? parseMessage input
    testCases = [ ("I 6 Completed armadillo processing", LogMessage Info 6 "Completed armadillo processing")
                , ("W 5 Flange is due for a check-up",   LogMessage Warning 5 "Flange is due for a check-up")
                , ("E 20 2 Too many pickles",            LogMessage (Error 20) 2 "Too many pickles")
                , ("abcdef",                             Unknown "abcdef") ]


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
                    Node Leaf (lmg 1) Leaf),                                              -- 1 element
                   ([lmg 1, lmg 3, lmg 2],
                    Node (Node Leaf (lmg 1) Leaf) (lmg 2) (Node Leaf (lmg 3) Leaf)),      -- 3 elements, evenly split
                   ([lmg 1, lmg 2, lmg 3],
                    Node (Node (Node Leaf (lmg 1) Leaf) (lmg 2) Leaf) (lmg 3) Leaf),      -- 3 elements, all left side
                   ([lmg 1, lmg 3, lmg 2],
                    Node (Node Leaf (lmg 1) Leaf) (lmg 2) (Node Leaf (lmg 3) Leaf)) ]     -- 3 elements, all right side

--- Exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lt m rt) = inOrder lt ++ [m] ++ inOrder rt

runInOrderTests :: IO Counts
runInOrderTests = runTestTT allTests
  where
    allTests :: Test
    allTests = TestList $ map baseTest testCases
    baseTest (input, expected) = expected ~=? inOrder input
    testCases :: [ (MessageTree, [LogMessage]) ]
    testCases =  [
                   (Node Leaf (lmg 1) Leaf,                                              -- 1 element
                    [lmg 1]),
                   (Node (Node Leaf (lmg 1) Leaf) (lmg 2) (Node Leaf (lmg 3) Leaf),      -- 3 elements, evenly split
                    [lmg 1, lmg 2, lmg 3]),
                   (Node (Node (Node Leaf (lmg 1) Leaf) (lmg 2) Leaf) (lmg 3) Leaf,      -- 3 elements, all left side
                    [lmg 1, lmg 2, lmg 3]),
                   (Node (Node Leaf (lmg 1) Leaf) (lmg 2) (Node Leaf (lmg 3) Leaf),      -- 3 elements, all right side
                   [lmg 1, lmg 2, lmg 3]) ]
