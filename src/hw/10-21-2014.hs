{-#OPTIONS_GHC -Wall -Werror #-}

import Data.Char

-----------------------------------------------------------------
-- Take String and return all lowercase characters in String
-----------------------------------------------------------------

removeCapitals :: String -> String
removeCapitals = filter isLower

-----------------------------------------------------------------
-- Take String and return all-capitalized String
-----------------------------------------------------------------

capitalize :: String -> String
capitalize = map toUpper
