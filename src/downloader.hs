{-#OPTIONS_GHC -Wall -Werror #-}

import Network.Curl.Download
import qualified Data.ByteString.Char8 as B

-- FIXME MOVE THIS OFF OF GITHUB BEFORE ADDING ANY KEYS

download :: IO ()
download = do
        doc <- openURI "http://www.crossfitreviver.com/index.php?format=feed&type=rss"
        either (putStr) (B.putStr) doc
--        tags <- openAsTags "http://www.crossfitreviver.com/index.php?format=feed&type=rss"
--        return tags
