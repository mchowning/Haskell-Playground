{-#OPTIONS_GHC -Wall -Werror #-}

import System.Environment
import Data.List


matchingLinesOnly :: String -> String -> String
matchingLinesOnly query input =
    let allLines = lines input
        matchingLines = filter (isInfixOf query) allLines
    in unlines matchingLines

main :: IO ()
main = do
    args <- getArgs
    case args of [query]           -> interact . matchingLinesOnly $ query
                 [query, filename] -> do contents <- readFile filename
                                         putStr $ matchingLinesOnly query contents
                 _                 -> putStrLn "Invalid use: must have 1 or 2 search parameters"
