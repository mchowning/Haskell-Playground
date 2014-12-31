{-#OPTIONS_GHC -Wall -Werror #-} 

import Data.List

runLength :: (Eq a) => [a] -> [(Int, a)]
runLength = map f . group 
    where
        f m = (length m, head m)

--runLength as = map (\x -> (length x, head x)) (group as)
