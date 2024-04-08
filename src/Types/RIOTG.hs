{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types.RIOTG where

import Data.Csv (FromNamedRecord (..), (.:))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding
import Data.Time.Calendar (Day)
import ShowText
import Types.Types
import Utils (dateConvert)

data RIOTG = RIOTG
  { riotgSubject :: Text,
    riotgSubjectCode :: String,
    riotgKPO :: Maybe Int,
    riotgSubjectFST :: Text,
    riotgSubjectFSTCode :: String,
    riotgSubjectFSTKPO :: Maybe Int,
    riotgStationName :: Text,
    riotgStationCode :: StationCode,
    riotgStationType :: StationType,
    riotgStationCategory :: StationCategory,
    riotgGTPName :: Text,
    riotgGTPCode :: String,
    riotgIsUnpriceZone :: Maybe Int,
    riotgPriceZone :: PriceZone,
    riotgPust :: Pust,
    riotgIsSpotTrader :: Bool,
    riotgIsExploitationTypeNormal :: Bool,
    riotgIsDPM :: Bool,
    riotgIsGaes :: Bool,
    riotgIsNewGesAes :: Bool,
    riotgXAttrType :: XAttrType,
    riotgDpmStartDate :: Maybe Day,
    riotgDpmFinishDate :: Maybe Day,
    riotgRegionRFCode :: Int,
    riotgOES :: Int,
    riotgZSP :: Maybe String,
    riotgRgeCode :: Int,
    riotgGaCode :: Int,
    riotgGaName :: Text,
    riotgGEM :: Maybe Int,
    riotgIsVR :: Bool,
    riotgVRStartDate :: Maybe Day,
    riotgVRFinishDate :: Maybe Day,
    riotgKommodStartDate :: Maybe Day,
    riotgKommodFinishDate :: Maybe Day,
    riotgIsKommodSelected :: Bool,
    riotgKommodSupplyStartDate :: Maybe Day,
    riotgIsVrZapret :: Bool,
    riotgIsVyvodSoglasovan :: Bool,
    riotgVyvodDate :: Maybe Day,
    riotgIsKomNgoSelected :: Bool,
    riotgNgoStartDate :: Maybe Day,
    riotgNgoFinishDate :: Maybe Day,
    riotgIsRprf2699 :: Bool,
    riotgRprf2699StartDate :: Maybe Day
  }
  deriving (Show)

instance FromNamedRecord RIOTG where
  parseNamedRecord record = do
    riotgSubject <- record .: encodeUtf8 "Участник"
    riotgSubjectCode <- record .: encodeUtf8 "Код участника"
    riotgKPO <- record .: encodeUtf8 "КПО"
    riotgSubjectFST <- record .: encodeUtf8 "Участник ФСТ"
    riotgSubjectFSTCode <- record .: encodeUtf8 "Код участника ФСТ"
    riotgSubjectFSTKPO <- record .: encodeUtf8 "КПО участника ФСТ"
    riotgStationName <- record .: encodeUtf8 "Станция"
    riotgStationCode <- record .: encodeUtf8 "Код станции"
    riotgStationType <- getStationType <$> record .: encodeUtf8 "Тип станции"
    riotgStationCategory <- getStationCategory <$> record .: encodeUtf8 "Категория станции"
    riotgGTPName <- record .: encodeUtf8 "ГТП"
    riotgGTPCode <- record .: encodeUtf8 "Код ГТП"
    riotgIsUnpriceZone <- record .: encodeUtf8 "Признак неценовой зоны"
    riotgPriceZone <- getPriceZone <$> record .: encodeUtf8 "Ценовая зона"
    riotgPust <- record .: encodeUtf8 "установленная мощность"
    riotgIsSpotTrader <- intTextToBool <$> record .: encodeUtf8 "выход_на_спот"
    riotgIsExploitationTypeNormal <- intTextToBool <$> record .: encodeUtf8 "характер_экспл"
    riotgIsDPM <- intTextToBool <$> record .: encodeUtf8 "IS_DPM"
    riotgIsGaes <- intTextToBool <$> record .: encodeUtf8 "IS_GAES"
    riotgIsNewGesAes <- intTextToBool <$> record .: encodeUtf8 "IS_NEW_GES_AES"
    riotgXAttrType <- getXAttrType <$> record .: encodeUtf8 "XATTR_TYPE"
    riotgDpmStartDate <- getDay <$> record .: "DPM_START_DATE"
    riotgDpmFinishDate <- getDay <$> record .: encodeUtf8 "DPM_FINISH_DATE"
    riotgRegionRFCode <- record .: encodeUtf8 "Регион_РФ"
    riotgOES <- record .: encodeUtf8 "ОЭС"
    riotgZSP <- record .: encodeUtf8 "ЗСП"
    riotgRgeCode <- record .: encodeUtf8 "Код РГЕ"
    riotgGaCode <- record .: encodeUtf8 "Код ГА"
    riotgGaName <- record .: encodeUtf8 "ГА"
    riotgGEM <- record .: encodeUtf8 "Принадлежность к ГЕМ"
    riotgIsVR <- intTextToBool <$> record .: encodeUtf8 "IS_VR"
    riotgVRStartDate <- getDay <$> record .: encodeUtf8 "ВР c"
    riotgVRFinishDate <- getDay <$> record .: encodeUtf8 "ВР до"
    riotgKommodStartDate <- getDay <$> record .: encodeUtf8 "KOMMOD_START_DATE"
    riotgKommodFinishDate <- getDay <$> record .: encodeUtf8 "KOMMOD_END_DATE"
    riotgIsKommodSelected <- intTextToBool <$> record .: encodeUtf8 "KOMMOD_SELECTED"
    riotgKommodSupplyStartDate <- getDay <$> record .: encodeUtf8 "KOMMOD_SUPPLY_START"
    riotgIsVrZapret <- intTextToBool <$> record .: encodeUtf8 "IS_VR_ZAPRET"
    riotgIsVyvodSoglasovan <- intTextToBool <$> record .: encodeUtf8 "IS_VUVOD_SOGLASOVAN"
    riotgVyvodDate <- getDay <$> record .: encodeUtf8 "DATA_VUVODA"
    riotgIsKomNgoSelected <- intTextToBool <$> record .: encodeUtf8 "IS_KOM_NGO_SELECTED"
    riotgNgoStartDate <- getDay <$> record .: encodeUtf8 "NGO_START_DATE"
    riotgNgoFinishDate <- getDay <$> record .: encodeUtf8 "NGO_FINISH_DATE"
    riotgIsRprf2699 <- intTextToBool <$> record .: encodeUtf8 "IS_RP_RF_2699"
    riotgRprf2699StartDate <- getDay <$> record .: encodeUtf8 "RPRF_GO_START_DATE"
    pure
      RIOTG
        { riotgSubject,
          riotgSubjectCode,
          riotgKPO,
          riotgSubjectFST,
          riotgSubjectFSTCode,
          riotgSubjectFSTKPO,
          riotgStationName,
          riotgStationCode,
          riotgStationType,
          riotgStationCategory,
          riotgGTPName,
          riotgGTPCode,
          riotgIsUnpriceZone,
          riotgPriceZone,
          riotgPust,
          riotgIsSpotTrader,
          riotgIsExploitationTypeNormal,
          riotgIsDPM,
          riotgIsGaes,
          riotgIsNewGesAes,
          riotgXAttrType,
          riotgDpmStartDate,
          riotgDpmFinishDate,
          riotgRegionRFCode,
          riotgOES,
          riotgZSP,
          riotgRgeCode,
          riotgGaCode,
          riotgGaName,
          riotgGEM,
          riotgIsVR,
          riotgVRStartDate,
          riotgVRFinishDate,
          riotgKommodStartDate,
          riotgKommodFinishDate,
          riotgIsKommodSelected,
          riotgKommodSupplyStartDate,
          riotgIsVrZapret,
          riotgIsVyvodSoglasovan,
          riotgVyvodDate,
          riotgIsKomNgoSelected,
          riotgNgoStartDate,
          riotgNgoFinishDate,
          riotgIsRprf2699,
          riotgRprf2699StartDate
        }
    where
      getStationCategory :: Text -> StationCategory
      getStationCategory "" = NormalStation
      getStationCategory "блок станция" = BlockStation
      getStationCategory other = error $ "getStationCategory wrong text: " <> Text.unpack other

      getStationType :: Text -> StationType
      getStationType "ТЭС" = TES
      getStationType "СЭС" = SES
      getStationType "ГЭС" = GES
      getStationType "ГРЭС(КЭС)-ТЭЦ" = GRES_KES_TEC
      getStationType "ВЭС" = VES
      getStationType "АЭС" = AES
      getStationType other = error $ "getStationType wrong text: " <> Text.unpack other

      getPriceZone :: Text -> PriceZone
      getPriceZone "EUR" = EUR
      getPriceZone "SIB" = SIB
      getPriceZone other = error $ "getPriceZone wrong text: " <> Text.unpack other

      getXAttrType :: Text -> XAttrType
      getXAttrType "" = EMPTY_XATTR_TYPE
      getXAttrType "is_renewable_dpm" = IS_RENEWABLE_DPM
      getXAttrType other = error $ "getXAttrType wrong text: " <> Text.unpack other

      --   getDay :: Text -> Maybe Day
      --   getDay txt = case readMaybe (Text.unpack txt) of
      --     Nothing -> traceShow txt Nothing
      --     res -> res

      getDay :: Text -> Maybe Day
      getDay "" = Nothing
      getDay txt = Just . read . Text.unpack $ dateConvert "mm/dd/yyyy" "yyyy-mm-dd" txt

      intTextToBool :: Text -> Bool
      intTextToBool "" = False
      intTextToBool "1" = True
      intTextToBool "0" = False
      intTextToBool other = error $ "intTextToBool wrong text: " <> Text.unpack other

instance ShowText RIOTG where
  showText
    RIOTG
      { riotgSubject,
        riotgSubjectCode,
        riotgKPO,
        riotgSubjectFST,
        riotgSubjectFSTCode,
        riotgSubjectFSTKPO,
        riotgStationName,
        riotgStationCode,
        riotgStationType,
        riotgStationCategory,
        riotgGTPName,
        riotgGTPCode,
        riotgIsUnpriceZone,
        riotgPriceZone,
        riotgPust,
        riotgIsSpotTrader,
        riotgIsExploitationTypeNormal,
        riotgIsDPM,
        riotgIsGaes,
        riotgIsNewGesAes,
        riotgXAttrType,
        riotgDpmStartDate,
        riotgDpmFinishDate,
        riotgRegionRFCode,
        riotgOES,
        riotgZSP,
        riotgRgeCode,
        riotgGaCode,
        riotgGaName,
        riotgGEM,
        riotgIsVR,
        riotgVRStartDate,
        riotgVRFinishDate,
        riotgKommodStartDate,
        riotgKommodFinishDate,
        riotgIsKommodSelected,
        riotgKommodSupplyStartDate,
        riotgIsVrZapret,
        riotgIsVyvodSoglasovan,
        riotgVyvodDate,
        riotgIsKomNgoSelected,
        riotgNgoStartDate,
        riotgNgoFinishDate,
        riotgIsRprf2699,
        riotgRprf2699StartDate
      } = "RIOTG {" <>
          "riotgSubject = \"" <> riotgSubject <> "\", " <>
          "riotgSubjectCode = " <> (Text.pack . show) riotgSubjectCode <> ", " <>
          "riotgKPO = " <> (Text.pack . show) riotgKPO <> ", " <>
          "riotgSubjectFST = \"" <> riotgSubjectFST <> "\", " <>
          "riotgSubjectFSTCode = " <> (Text.pack . show) riotgSubjectFSTCode <> ", " <>
          "riotgSubjectFSTKPO = " <> (Text.pack . show) riotgSubjectFSTKPO <> ", " <>
          "riotgStationName = \"" <> riotgStationName <> "\", " <>
          "riotgStationCode = " <> (Text.pack . show) riotgStationCode <> ", " <>
          "riotgStationType = " <> (Text.pack . show) riotgStationType <> ", " <>
          "riotgStationCategory = " <> (Text.pack . show) riotgStationCategory <> ", " <>
          "riotgGTPName = \"" <> riotgGTPName <> "\", " <>
          "riotgGTPCode = " <> (Text.pack . show) riotgGTPCode <> ", " <>
          "riotgIsUnpriceZone = " <> (Text.pack . show) riotgIsUnpriceZone <> ", " <>
          "riotgPriceZone = " <> (Text.pack . show) riotgPriceZone <> ", " <>
          "riotgPust = " <> (Text.pack . show) riotgPust <> ", " <>
          "riotgIsSpotTrader = " <> (Text.pack . show) riotgIsSpotTrader <> ", " <>
          "riotgIsExploitationTypeNormal = " <> (Text.pack . show) riotgIsExploitationTypeNormal <> ", " <>
          "riotgIsDPM = " <> (Text.pack . show) riotgIsDPM <> ", " <>
          "riotgIsGaes = " <> (Text.pack . show) riotgIsGaes <> ", " <>
          "riotgIsNewGesAes = " <> (Text.pack . show) riotgIsNewGesAes <> ", " <>
          "riotgXAttrType = " <> (Text.pack . show) riotgXAttrType <> ", " <>
          "riotgDpmStartDate = " <> (Text.pack . show) riotgDpmStartDate <> ", " <>
          "riotgDpmFinishDate = " <> (Text.pack . show) riotgDpmFinishDate <> ", " <>
          "riotgRegionRFCode = " <> (Text.pack . show) riotgRegionRFCode <> ", " <>
          "riotgOES = " <> (Text.pack . show) riotgOES <> ", " <>
          "riotgZSP = " <> (Text.pack . show) riotgZSP <> ", " <>
          "riotgRgeCode = " <> (Text.pack . show) riotgRgeCode <> ", " <>
          "riotgGaCode = " <> (Text.pack . show) riotgGaCode <> ", " <>
          "riotgGaName = \"" <> riotgGaName <> "\", " <>
          "riotgGEM = " <> (Text.pack . show) riotgGEM <> ", " <>
          "riotgIsVR = " <> (Text.pack . show) riotgIsVR <> ", " <>
          "riotgVRStartDate = " <> (Text.pack . show) riotgVRStartDate <> ", " <>
          "riotgVRFinishDate = " <> (Text.pack . show) riotgVRFinishDate <> ", " <>
          "riotgKommodStartDate = " <> (Text.pack . show) riotgKommodStartDate <> ", " <>
          "riotgKommodFinishDate = " <> (Text.pack . show) riotgKommodFinishDate <> ", " <>
          "riotgIsKommodSelected = " <> (Text.pack . show) riotgIsKommodSelected <> ", " <>
          "riotgKommodSupplyStartDate = " <> (Text.pack . show) riotgKommodSupplyStartDate <> ", " <>
          "riotgIsVrZapret = " <> (Text.pack . show) riotgIsVrZapret <> ", " <>
          "riotgIsVyvodSoglasovan = " <> (Text.pack . show) riotgIsVyvodSoglasovan <> ", " <>
          "riotgVyvodDate = " <> (Text.pack . show) riotgVyvodDate <> ", " <>
          "riotgIsKomNgoSelected = " <> (Text.pack . show) riotgIsKomNgoSelected <> ", " <>
          "riotgNgoStartDate = " <> (Text.pack . show) riotgNgoStartDate <> ", " <>
          "riotgNgoFinishDate = " <> (Text.pack . show) riotgNgoFinishDate <> ", " <>
          "riotgIsRprf2699 = " <> (Text.pack . show) riotgIsRprf2699 <> ", " <>
          "riotgRprf2699StartDate = " <> (Text.pack . show) riotgRprf2699StartDate <>
          "}"