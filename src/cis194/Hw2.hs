{-#OPTIONS_GHC -Wall -Werror #-}
module Hw2 where

import Hw2Log

parseMessage :: String -> LogMessage
parseMessage s = LogMessage messageType timestamp message
  where
    ls = words s
    messageType = getMessageType ls
    timestamp = getTimeStamp ls
    message = getMessage ls

getMessageType :: [String] -> MessageType
getMessageType ("E":l:_) = Error (read l)
getMessageType ("I":_)   = Info
getMessageType ("W":_)   = Warning
getMessageType _         = undefined

getTimeStamp :: [String] -> TimeStamp
getTimeStamp = read . head . removeMessageType

getMessage :: [String] -> String
getMessage = unwords . drop 1 . removeMessageType

removeMessageType :: [String] -> [String]
removeMessageType xs@("E":_) = drop 2 xs
removeMessageType xs         = drop 1 xs


parse :: String -> [LogMessage]
parse = map parseMessage . lines

iTest :: String
iTest = "I 6 Completed armadillo processing"
wTest :: String
wTest = "W 5 Flange is due for a check-up"
eTest :: String
eTest = "E 20 2 Too many pickles"

