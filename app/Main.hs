module Main (main) where

import Configuration.Dotenv (parseFile)
import LoadInputData
import PrintUtf8

import Data.ByteString.UTF8 (fromString)
-- import Prelude hiding (putStr, putStrLn)
import qualified Data.ByteString.Char8 as C8 -- (putStr, putStrLn)
import Types.KOMTG (KOMTG(..))

main :: IO ()
main = do
  env <- parseFile "config.env"

  inputData <- loadInputData env


  -- mapM_ (\x -> C8.putStr (komtgStationName x) >> (C8.putStr $ fromString $ show x) >> putStr "\n")  (take 35 $ datKOMTG inputData)
  -- mapM_ (\x -> C8.putStr (komtgStationName  x) >> putStr "\n")  (take 35 $ datKOMTG inputData)

  mapM_ (\x -> (C8.putStr $ fromString $ show x) >> putStr "\n")  (take 35 $ datSoRegistry inputData)

  putStrLnUtf8 "Хорошо! čušpajž日本語"

  -- print $ routine inputData
  pure ()

-- env <- parseFile "config.env"
-- print (mkYearDate env)
-- putStrLn "\n"

-- Right rio <- loadRIOTG "input_data/RIO_TG.csv"
-- traverse_ uprint (V.take 20 $ snd rio)
-- putStrLn "\n"

-- Right vr7_11 <- loadExploitationStartYear "input_data/GA_2007-2011.csv"
-- traverse_ uprint (V.take 20 $ snd vr7_11)
-- putStrLn "\n"

-- Right soReg <- loadSoRegistry "input_data/SO_REGISTRY.csv"
-- traverse_ uprint (snd soReg)
-- putStrLn "\n"

-- Right komTG <- loadKOMTG "input_data/KOM_PO_TG.csv"
-- traverse_ uprint (snd komTG)
-- putStrLn "\n"

-- ushow
