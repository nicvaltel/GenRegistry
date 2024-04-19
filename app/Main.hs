module Main (main) where

import Configuration.Dotenv (parseFile)
import Control.Monad (forM_)
import Control.Monad.Writer (runWriter)
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.UTF8 (fromString)
import Data.List (nub, sort)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as T
import LoadInputData
import Routine (routine)
import Types.KOMTG (KOMTG (..))
import Utils

main :: IO ()
main = do
  env <- parseFile "config.env"

  inputData <- loadInputData env

  -- mapM_ (\x -> C8.putStr (komtgStationName x) >> (C8.putStr $ fromString $ show x) >> putStr "\n")  (take 35 $ datKOMTG inputData)
  -- mapM_ (\x -> C8.putStr (komtgStationName  x) >> putStr "\n")  (take 35 $ datKOMTG inputData)
  -- mapM_ (putTextLnUtf8 . komtgStationName)  (take 35 $ datKOMTG inputData)

  -- mapM_ (\x -> (C8.putStr $ fromString $ show x) >> putStr "\n")  (take 35 $ datSoRegistry inputData)

  -- mapM_ (putTextLnUtf8 . showText)  (take 35 $ datRIOTG inputData)

  let (result, warnings) = runWriter (routine inputData)

  putStrLn "\n**************** WARNINGS ****************\n"
  forM_ (sort $ nub warnings) $ \w -> print w

  putStrLn "\n***************** RESULT *****************\n"
  forM_ result $ \r -> print r
