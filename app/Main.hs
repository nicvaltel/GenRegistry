{-# LANGUAGE OverloadedStrings #-}  
module Main (main) where

import Configuration.Dotenv (parseFile)
import Control.Monad (forM_)
import Control.Monad.Writer (runWriter)
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.UTF8 (fromString)
import Data.List (nub, sort)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as T
import qualified Data.Text as Text
import LoadInputData
import Routine (routine)
import Types.KOMTG (KOMTG (..))
import Utils
import Types.GeneratorEntry (GeneratorEntry(..), generatorEntryToTextList, genEntryToCsvFormat)
import Types.Types (SupplyAttribute(..), ConstantsAndDates (..), ShowText (showText))
import TextShow (printT)
import Control.Monad.RWS (runRWS)

main :: IO ()
main = do
  env <- parseFile "config.env"

  inputData <- loadInputData env
  -- mapM_ (putTextLnUtf8 . showText)  (take 35 $ datRIOTG inputData)

  let (result, _, warnings) = runRWS (routine inputData) (datConstantsAndDates inputData) ()

  putStrLn "\n**************** WARNINGS ****************\n"
  forM_ (sort $ nub warnings) $ \w -> print w

  putStrLn "\n***************** RESULT *****************\n"

  -- forM_ result $ \r -> print r
  -- forM_ result $ \r -> putTextLnUtf8 $ Text.pack $ show $ grSubject r
  -- printT $ SupplyPeriod {supplyPeriodFrom = cndStartYearDate (datConstantsAndDates inputData), supplyPeriodTo = cndStartYearDate (datConstantsAndDates inputData)}

  putTextLnUtf8 $ genEntryToCsvFormat result

