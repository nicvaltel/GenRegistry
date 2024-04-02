module Main (main) where

import Configuration.Dotenv (parseFile)
import Data.Foldable (traverse_)
import qualified Data.Vector as V
import LoadInputData
import Text.Show.Unicode
import Routine (routine)

main :: IO ()
main = do
  env <- parseFile "config.env"

  inputData <- loadInputData env

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
