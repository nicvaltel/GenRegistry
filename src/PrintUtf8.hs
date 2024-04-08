-- {-# LANGUAGE OverloadedStrings #-}
module PrintUtf8 (putStrUtf8, putStrLnUtf8) where


import Data.ByteString.UTF8 (fromString)
import Prelude hiding (putStr, putStrLn)
import Data.ByteString.Char8 (putStr, putStrLn)

putStrUtf8 :: String -> IO ()
putStrUtf8 = putStr . fromString

putStrLnUtf8 :: String -> IO ()
putStrLnUtf8 = putStrLn . fromString


-- import qualified Data.Text    as T
-- import qualified Data.Text.IO as T

-- putStrUtf8 :: String -> IO ()
-- putStrUtf8 = T.putStr . T.pack

-- putStrLnUtf8 :: String -> IO ()
-- putStrLnUtf8 = T.putStrLn . T.pack