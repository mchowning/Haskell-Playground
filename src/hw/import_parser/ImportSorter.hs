{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import System.Environment
import Data.Ord (comparing)
import Data.List (sortBy)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.CaseInsensitive as CI

main :: IO ()
main = do
    filename <- head <$> getArgs
    contents <- T.readFile filename

    let (preImportLines, importLines, postImportLines) = separateImportLines contents
    let sortedImports = sortImports importLines

    -- todo remove
    T.putStrLn "PREIMPORT LINES:"
    mapM_ T.putStrLn preImportLines
    T.putStrLn "IMPORT LINES:"
    mapM_ T.putStrLn sortedImports
    T.putStrLn "POST IMPORT LINES:"
    mapM_ T.putStrLn postImportLines
    -----------------------------------------------------------

    let newFileContents = T.unpack . T.unlines $ preImportLines ++ sortedImports ++ (T.singleton '\n' : postImportLines)

    writeFile "testOutput.txt" newFileContents
--     writeFile filename newFileContents
    -- todo uncomment real line

  where
    separateImportLines :: T.Text -> ([T.Text], [T.Text], [T.Text])
    separateImportLines t = (preImportLines, importLines, postImportLines)
      where
        (preImportLines, rest) = break (isImportLine False) (T.lines t)
        (importLines, postImportLines) = span (isImportLine True) rest

        isImportLine :: Bool -> T.Text -> Bool
        isImportLine onBlank "" = onBlank
        isImportLine onBlank aLine | isBlankLine aLine = onBlank
                                   | otherwise         = "import" == head (T.words aLine)

    sortImports :: [T.Text] -> [T.Text]
    sortImports = sortBy orderCaseInsensitiveSecondElement . removeEmptyLines
      where
        orderCaseInsensitiveSecondElement :: T.Text -> T.Text -> Ordering
        orderCaseInsensitiveSecondElement = comparing (CI.mk . getSecondElement)
          where
            getSecondElement :: T.Text -> T.Text
            getSecondElement fullLine | length ws > 1 = ws !! 1
                                      | otherwise     = ""
              where
                ws = T.words fullLine

        removeEmptyLines :: [T.Text] -> [T.Text]
        removeEmptyLines = filter (not . isBlankLine)

    isBlankLine :: T.Text -> Bool
    isBlankLine = T.foldr (\i acc -> acc && (i == ' ' || i == '\n')) True



