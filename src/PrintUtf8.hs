module PrintUtf8 (putStrUtf8, putStrLnUtf8, putTextUtf8, putTextLnUtf8) where


import Data.ByteString.UTF8 (fromString)
import Prelude hiding (putStr, putStrLn)
import qualified Data.ByteString.Char8 as C8 (putStr, putStrLn)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

putStrUtf8 :: String -> IO ()
putStrUtf8 = C8.putStr . fromString

putStrLnUtf8 :: String -> IO ()
putStrLnUtf8 = C8.putStrLn . fromString



putTextUtf8 :: Text -> IO ()
putTextUtf8 = C8.putStr . encodeUtf8

putTextLnUtf8 :: Text -> IO ()
putTextLnUtf8 = C8.putStrLn . encodeUtf8