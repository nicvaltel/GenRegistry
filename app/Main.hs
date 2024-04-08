module Main (main) where

import Configuration.Dotenv (parseFile)
import LoadInputData
import PrintUtf8
import qualified Data.Text.IO as T

import Data.ByteString.UTF8 (fromString)
import qualified Data.ByteString.Char8 as C8 -- (putStr, putStrLn)
import Types.KOMTG (KOMTG(..))
import Data.Text.Encoding (encodeUtf8)
import ShowText (showText)

main :: IO ()
main = do
  env <- parseFile "config.env"

  inputData <- loadInputData env


  -- mapM_ (\x -> C8.putStr (komtgStationName x) >> (C8.putStr $ fromString $ show x) >> putStr "\n")  (take 35 $ datKOMTG inputData)
  -- mapM_ (\x -> C8.putStr (komtgStationName  x) >> putStr "\n")  (take 35 $ datKOMTG inputData)
  -- mapM_ (putTextLnUtf8 . komtgStationName)  (take 35 $ datKOMTG inputData)

  -- mapM_ (\x -> (C8.putStr $ fromString $ show x) >> putStr "\n")  (take 35 $ datSoRegistry inputData)
  
  mapM_ (putTextLnUtf8 . showText)  (take 35 $ datRIOTG inputData)


  -- print $ routine inputData
  -- pure ()
