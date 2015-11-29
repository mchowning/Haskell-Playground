{-# OPTIONS_GHC -Wall -Werror #-}

module Hw2_2 where

import Log
import Test.HUnit

tests :: IO Counts
tests = runTestTT . TestList $
  parseMessageTests
  ++ insertTests

-------------------- Exercise 1 --------------------

parseMessage :: String -> LogMessage
parseMessage [] = Unknown ""
parseMessage s  =
  case words s of
    "E":errorLevel:timestamp:message -> LogMessage (Error (read errorLevel)) (read timestamp) (unwords message)
    "W":timestamp:message            -> LogMessage Warning (read timestamp) (unwords message)
    "I":timestamp:message            -> LogMessage Info (read timestamp) (unwords message)
    message                          -> Unknown (unwords message)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

parseMessageTests :: [Test]
parseMessageTests = [ "parseMessage \"E 2 562 help help\"" ~:
                       parseMessage "E 2 562 help help" ~?= LogMessage (Error 2) 562 "help help"
                    , "parseMessage \"I 29 la la la\"" ~:
                       parseMessage "I 29 la la la" ~?= LogMessage Info 29 "la la la"
                    , "parseMessage \"W 3 la la la\"" ~:
                       parseMessage "W 3 la la la" ~?= LogMessage Warning 3 "la la la"
                    , "parseMessage \"This is not in the right format\"" ~:
                       parseMessage "This is not in the right format" ~?= Unknown "This is not in the right format"
--                     , "parseMessage \"E This is not in the right format\"" ~:
--                        parseMessage "E This is not in the right format" ~?= Unknown "E This is not in the right format"
                    ]

-------------------- Exercise 2 --------------------

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt                   = mt
insert lm Leaf                          = Node Leaf lm Leaf
insert lm@(LogMessage _ ntimestamp _) (Node left center@(LogMessage _ ctimestamp _) right)
  | ntimestamp < ctimestamp = Node (insert lm left) center right
  | otherwise               = Node left center (insert lm right)
insert _ _ = undefined

insertTests :: [Test]
insertTests = [ "insert Unknown" ~:
                 insert (Unknown "") Leaf ~?= Leaf
              , "insert into Leaf" ~:
                 insert (testInfoLogMessage 1) Leaf ~?= Node Leaf (testInfoLogMessage 1) Leaf
              , "insert less" ~:
                 insert (testInfoLogMessage 49) (Node Leaf (testInfoLogMessage 50) Leaf) ~?=
                 Node (Node Leaf (testInfoLogMessage 49) Leaf) (testInfoLogMessage 50) Leaf
              , "insert more" ~:
                 insert (testInfoLogMessage 51) (Node Leaf (testInfoLogMessage 50) Leaf) ~?=
                 Node Leaf (testInfoLogMessage 50) (Node Leaf (testInfoLogMessage 51) Leaf)
              ]
    where
      testInfoLogMessage :: Int -> LogMessage
      testInfoLogMessage n = LogMessage Info n "message"

-------------------- Exercise 3 --------------------

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-------------------- Exercise 4 --------------------

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left lm right) = inOrder left ++ [lm] ++ inOrder right


warnLogMessage :: Int -> LogMessage
warnLogMessage n = LogMessage Warning n ""

errorLogMessage :: Int -> Int -> LogMessage
errorLogMessage level time = LogMessage (Error level) time ""

-------------------- Exercise 5 --------------------

whatWentWrong :: [LogMessage] -> [String]
-- whatWentWrong :: [LogMessage] -> [LogMessage]
whatWentWrong = map showMessage . inOrder . build . filter (\x -> hasMinErrorLevel 50 x && isError x)
  where
    isError :: LogMessage -> Bool
    isError (LogMessage (Error _) _ _) = True
    isError _                          = False

    hasMinErrorLevel :: Int -> LogMessage -> Bool
    hasMinErrorLevel n (LogMessage (Error level) _ _) = n < level
    hasMinErrorLevel _ _                              = False

    showMessage :: LogMessage -> String
    showMessage (LogMessage _ _ message) = message
    showMessage _                        = ""