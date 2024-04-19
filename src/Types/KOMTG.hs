{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types.KOMTG (KOMTG (..)) where

import Data.Csv (FromNamedRecord (..), (.:))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding
import Types.Types

data KOMTG = KOMTG
  { -- komtgFilialODU :: String,
    -- komtgPriseZone :: PriceZone,
    komtgSubjectCode :: String,
    -- komtgSubjectName :: String,
    -- komtgSubjectFullName :: String,
    komtgStationCode :: String,
    komtgStationName :: Text,
    komtgGTPGCode :: String,
    -- komtgGTPGName :: String,
    komtgGEMCode :: String,
    -- komtgGEMName :: String,
    komtgStationType :: StationType,
    -- komtgEGOName :: String,
    komtgEGOCode :: GaCode,
    komtgDPMCode :: Maybe Int,
    komtgMVR :: Bool,
    komtgJan :: Maybe Float,
    komtgFeb :: Maybe Float,
    komtgMar :: Maybe Float,
    komtgApr :: Maybe Float,
    komtgMay :: Maybe Float,
    komtgJun :: Maybe Float,
    komtgJul :: Maybe Float,
    komtgAug :: Maybe Float,
    komtgSep :: Maybe Float,
    komtgOct :: Maybe Float,
    komtgNov :: Maybe Float,
    komtgDec :: Maybe Float,
    komtgRefusalToFillRequest :: Bool,
    komtgGemSelectionResult :: GemSelectionResult,
    komtgMVRType :: Maybe MVRType,
    komtgYearSum :: Float
  }
  deriving (Show)

instance FromNamedRecord KOMTG where
  parseNamedRecord record = do
    -- komtgFilialODU <- record .: encodeUtf8 "Филиал ОДУ"
    -- komtgPriseZone <- getPriceZone <$> record .: encodeUtf8 "Код ЦЗ"
    komtgSubjectCode <- record .: encodeUtf8 "Код участника оптового рынка"
    -- komtgSubjectName <- record .: encodeUtf8 "Участник оптового рынка"
    -- komtgSubjectFullName <- record .: encodeUtf8 "Участник оптового рынка (полное наименование)"
    komtgStationCode <- record .: encodeUtf8 "Код электростанции"
    komtgStationName <- Text.pack <$> record .: encodeUtf8 "Наименование электростанции"
    komtgGTPGCode <- record .: encodeUtf8 "Код ГТПГ"
    -- komtgGTPGName <- record .: encodeUtf8 "Наименование ГТПГ"
    komtgGEMCode <- record .: encodeUtf8 "Код ГЕМ"
    -- komtgGEMName <- record .: encodeUtf8 "ГЕМ"
    komtgStationType <- getStationType <$> record .: encodeUtf8 "Тип э/ст"
    -- komtgEGOName <- record .: encodeUtf8 "Наименование ЕГО"
    komtgEGOCode <- record .: encodeUtf8 "КОД ЕГО"
    komtgDPMCode <- record .: encodeUtf8 "ДПМ"
    komtgMVR <- intTextToBool <$> record .: encodeUtf8 "МВР"
    komtgJan <- record .: encodeUtf8 "январь"
    komtgFeb <- record .: encodeUtf8 "февраль"
    komtgMar <- record .: encodeUtf8 "март"
    komtgApr <- record .: encodeUtf8 "апрель"
    komtgMay <- record .: encodeUtf8 "май"
    komtgJun <- record .: encodeUtf8 "июнь"
    komtgJul <- record .: encodeUtf8 "июль"
    komtgAug <- record .: encodeUtf8 "август"
    komtgSep <- record .: encodeUtf8 "сентябрь"
    komtgOct <- record .: encodeUtf8 "октябрь"
    komtgNov <- record .: encodeUtf8 "ноябрь"
    komtgDec <- record .: encodeUtf8 "декабрь"
    komtgRefusalToFillRequest <- intTextToBool <$> record .: encodeUtf8 "Отказ от заполнения"
    komtgGemSelectionResult <- getGemSelectionResult <$> record .: encodeUtf8 "Результат отбора по ГЕМ"
    komtgMVRType <- getMVRTYpe <$> record .: encodeUtf8 "Тип МВР_(1 - ЭЭ; 2 - тепло)"
    komtgYearSum <- record .: encodeUtf8 "F32"
    pure
      KOMTG
        { -- komtgFilialODU,
          -- komtgPriseZone,
          komtgSubjectCode,
          -- komtgSubjectName,
          -- komtgSubjectFullName,
          komtgStationCode,
          komtgStationName,
          komtgGTPGCode,
          -- komtgGTPGName,
          komtgGEMCode,
          -- komtgGEMName,
          komtgStationType,
          -- komtgEGOName,
          komtgEGOCode,
          komtgDPMCode,
          komtgMVR,
          komtgJan,
          komtgFeb,
          komtgMar,
          komtgApr,
          komtgMay,
          komtgJun,
          komtgJul,
          komtgAug,
          komtgSep,
          komtgOct,
          komtgNov,
          komtgDec,
          komtgRefusalToFillRequest,
          komtgGemSelectionResult,
          komtgMVRType,
          komtgYearSum
        }
    where
      -- getPriceZone :: Text -> PriceZone
      -- getPriceZone "1" = EUR
      -- getPriceZone "2" = SIB
      -- getPriceZone other = error $ "getPriceZone wrong text: " <> Text.unpack other

      getStationType :: Text -> StationType
      getStationType "1" = TES
      getStationType "2" = GES
      getStationType "3" = AES
      getStationType "4" = GRES_KES_TEC
      getStationType other = error $ "getStationType wrong text: " <> Text.unpack other

      intTextToBool :: Text -> Bool
      intTextToBool "" = False
      intTextToBool "1" = True
      intTextToBool "0" = False
      intTextToBool other = error $ "intTextToBool wrong text: " <> Text.unpack other

      getGemSelectionResult :: Text -> GemSelectionResult
      getGemSelectionResult "Отобрана" = GSRSelected
      getGemSelectionResult "Не отобрана" = GSRNotSelected
      getGemSelectionResult "Не отобрана, поставка после модернизации по новой ГЕМ" = GSRNotSelectedSupplyAfterMod
      getGemSelectionResult "Отказ от заполнения заявки" = GSRNotSelectedRefusalToFillRequest
      getGemSelectionResult "Не отобрана, не соотв.мин.тех.требованиям" = GSRNotSelectedMinimumTechnicalRequirements
      getGemSelectionResult "Не отобрана, не предоставлено обеспечение" = GSRNotSelectedNoTechnicalSupport
      getGemSelectionResult "Не отобрана, ноль в декабре" = GSRNotSelectedZeroInDecember
      getGemSelectionResult "МВР" = GSRMVR
      getGemSelectionResult "ДПМ" = GSRDPM
      getGemSelectionResult other = error $ "getGemSelectionResult wrong text: " <> Text.unpack other

      getMVRTYpe :: Text -> Maybe MVRType
      getMVRTYpe "" = Nothing
      getMVRTYpe "1" = Just MVR_EE
      getMVRTYpe "2" = Just MVR_HEAT
      getMVRTYpe other = error $ "getMVRTYpe wrong text: " <> Text.unpack other
