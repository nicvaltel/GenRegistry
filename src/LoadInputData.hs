{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LoadInputData where

import qualified Data.ByteString.Lazy as BL
import Data.Csv (FromNamedRecord, Header, decodeByName)
import Data.Time (fromGregorian)
import Data.Vector (toList)
import qualified Data.Vector as V
import Text.Read (readMaybe)
import Types.KOMTG (KOMTG)
import Types.RIOTG ( RIOTG )
import Types.SoRegistry (SoRegistry)
import Types.Types ( YearDate(..) )
import Types.ExploitationStartYear (ExploitationStartYear)
import Data.Either.Combinators(maybeToRight)

type ErrorMsg = String

type CsvData a = (Header, V.Vector a)

data InputData = InputData
  { datRIOTG :: [RIOTG],
    datKOMTG :: [KOMTG],
    datSoRegistry :: [SoRegistry],
    datExploitationStartYear :: [ExploitationStartYear],
    datYearDate :: YearDate
  } deriving (Show)

parseCSV :: FromNamedRecord a => FilePath -> IO (Either ErrorMsg (CsvData a))
parseCSV filePath = do
  contents <- BL.readFile filePath
  return $ decodeByName contents

-- Discard headers from CsvData
removeHeaders :: CsvData a -> V.Vector a
removeHeaders = snd

loadRIOTG :: FilePath -> IO (Either ErrorMsg (CsvData RIOTG))
loadRIOTG = parseCSV

loadExploitationStartYear :: FilePath -> IO (Either ErrorMsg (CsvData ExploitationStartYear))
loadExploitationStartYear = parseCSV

loadSoRegistry :: FilePath -> IO (Either ErrorMsg (CsvData SoRegistry))
loadSoRegistry = parseCSV

loadKOMTG :: FilePath -> IO (Either ErrorMsg (CsvData KOMTG))
loadKOMTG = parseCSV

mkYearDate :: [(String, String)] -> YearDate
mkYearDate env =
  case readMaybe <$> lookup "MIN_PUST" env of
    Just (Just (minPust :: Float)) ->
      case readMaybe <$> lookup "YEAR" env of
        Just (Just (year :: Integer)) ->
          YearDate
            { ydYear = fromIntegral year,
              yd_VrBanDate = fromGregorian (year - 1) 10 15,
              ydStartYearDate = fromGregorian year 1 1,
              ydFinishYearDate = fromGregorian year 12 31,
              ydFirstApril = fromGregorian year 4 1,
              ydSecondTermDate = fromGregorian year 7 1,
              ydMinPust = minPust
            }
        _ -> error "YEAR field is abscent or incorrect in config.env file"
    _ -> error "MIN_PUST field is abscent or incorrect in config.env file"

loadInputData :: [(String, String)] -> IO InputData
loadInputData env = do
  let datYearDate = mkYearDate env
  let mbPathes = do  
                rioTG <- maybeToRight "Error: no INPUT_RIO_TG filepath in config.env" (lookup "INPUT_RIO_TG" env)
                komTG <- maybeToRight "Error: no INPUT_KOM_PO_TG filepath in config.env" (lookup "INPUT_KOM_PO_TG" env)
                soReg <- maybeToRight "Error: no INPUT_SO_REGISTRY filepath in config.env" (lookup "INPUT_SO_REGISTRY" env)
                explYear <- maybeToRight "Error: no INPUT_GA_2007_2011 filepath in config.env" (lookup "INPUT_GA_2007_2011" env)
                pure (rioTG, komTG, soReg, explYear)
  case mbPathes of
    Left errMsg -> error errMsg
    Right (rioTG, komTG, soReg, explYear) -> do
      Right datRIOTG <- (toList . snd <$>) <$> loadRIOTG rioTG
      Right datExploitationStartYear <- (toList . snd <$>) <$> loadExploitationStartYear explYear
      Right datSoRegistry <- (toList . snd <$>) <$> loadSoRegistry soReg
      Right datKOMTG <- (toList . snd <$>) <$> loadKOMTG komTG
      pure InputData {datRIOTG, datKOMTG, datSoRegistry, datExploitationStartYear, datYearDate}
