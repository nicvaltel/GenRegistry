{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.GeneratorEntry (GeneratorEntry (..), generatorEntryToTextList, genEntryToCsvFormat) where

import Data.Text (Text, pack)
import qualified Data.Text as Text
import Data.Time.Calendar (Day)
import Types.Types
import Utils (tMayShow, tshow)

data GeneratorEntry = GeneratorEntry
  { grSubject :: Text, -- Участник
    grSubjectCode :: String, -- Код участника
    grSubjectFST :: Text, -- Участник ФСТ
    grSubjectFSTCode :: String, -- Код участника ФСТ
    grStationName :: Text, -- Станция
    grStationCode :: StationCode, -- Код станции
    grStationType :: StationType, -- Тип станции
    grGTPGname :: Text, -- ГТП
    grGTPGCode :: GTPGCode, -- Код ГТП
    grUnpriceZoneCode :: Maybe Int, -- Признак неценовой зоны
    grPriceZone :: PriceZone, -- Ценовая зона
    grPustGTPG :: Pust, -- установленная мощность ГТПГ
    grIsSpotTrader :: Bool, -- выход_на_спот
    grIsExploitationTypeNormal :: Bool, -- характер_экспл
    grIsDPM :: Bool, -- IS_DPM
    grRegionRFCode :: Int, -- Регион_РФ
    grOES :: Int, -- ОЭС
    grZSP :: Maybe String, -- ЗСП
    grRGECode :: Int, -- Код РГЕ
    grGACode :: Int, -- Код ГА
    grGaName :: Text, -- ГА
    grGEM :: Maybe Int, -- Принадлежность к ГЕМ
    grGemSelectionResult :: GemSelectionResult, -- Результат отбора по ГЕМ
    grVR_KOM :: Bool, -- ВР_КОМ
    grNotSelectedInKOM :: Bool, -- Не отобрано
    grKOMRequestAbsent :: Bool, -- Нет_заявки_КОМ
    grVRNotFromTheYearBegin :: Maybe Bool, -- ВР_не_с_начала_года
    grVRNotUntilTheYearEnd :: Maybe Bool, -- ВР_не_до_конца_года
    grVRNotCondideredInKOM :: Maybe Bool, -- ВР_не_учтён_в_КОМ
    grDpmStartDate :: Maybe Day, -- Дата_начала_поставки_ДПМ
    grDpmFinishDate :: Maybe Day, -- Дата окончания поставки ДПМ (новое поле)
    grVRProhibitDecisionDate :: Maybe Day, -- Дата_решения_о_запрете(ВР)
    grKOM :: Bool, -- Группа_КОМ
    grKOMMOD :: Bool, -- Группа_КОММОД
    grDPM :: Bool, -- Группа_ДПМ
    grNGO :: Bool, -- Группа_НГО
    grRPRF_2699 :: Bool, -- Группа_РПРФ2699p
    grVRwithAllYearCapacity :: Maybe Bool, -- Группа_ВР_с_МЩ_весь_год
    grVR_2007_2011 :: Maybe Bool, -- Группа_ВР_2007-2011
    grVRafter15october :: Maybe Bool, -- Группа_ВР_после_15_октября
    grVRNotAllYear :: Maybe Bool, -- Группа_ВР_не_весь_год
    grEESupply :: [SupplyAttribute 'EE], -- Поставка_ЭЭ_по_РД
    grPWSupply :: [SupplyAttribute 'PW], -- Поставка_МЩ_по_РД
    grPustStation :: Pust, -- Руст_станции
    grIsNewGesAes :: Bool, -- IS_NEW_GES_AES
    grIsVr :: Bool, -- IS_VR
    grVrFrom :: Maybe Day, -- ВР c
    grVrTo :: Maybe Day, -- ВР до
    grKOMMODModernizationStartDate :: Maybe Day, -- KOMMOD_START_DATE
    grKOMMODModernizationEndDate :: Maybe Day, -- KOMMOD_END_DATE
    grIsKOMMODSelected :: Bool, -- KOMMOD_SELECTED
    grKOMMODSupplyStart :: Maybe Day, -- KOMMOD_SUPPLY_START
    grIsVRProhibit :: Maybe Bool, -- IS_VR_ZAPRET
    grIsVyvodSoglasovan :: Bool, -- IS_VUVOD_SOGLASOVAN
    grVyvodSoglasovanDate :: Maybe Day, -- DATA_VUVODA
    grComment :: [Text] -- Комментарий
  }
  deriving (Show)

generatorEntryToTextList :: GeneratorEntry -> [Text]
generatorEntryToTextList
  GeneratorEntry
    { grSubject,
      grSubjectCode,
      grSubjectFST,
      grSubjectFSTCode,
      grStationName,
      grStationCode,
      grStationType,
      grGTPGname,
      grGTPGCode,
      grUnpriceZoneCode,
      grPriceZone,
      grPustGTPG,
      grIsSpotTrader,
      grIsExploitationTypeNormal,
      grIsDPM,
      grRegionRFCode,
      grOES,
      grZSP,
      grRGECode,
      grGACode,
      grGaName,
      grGEM,
      grGemSelectionResult,
      grVR_KOM,
      grNotSelectedInKOM,
      grKOMRequestAbsent,
      grVRNotFromTheYearBegin,
      grVRNotUntilTheYearEnd,
      grVRNotCondideredInKOM,
      grDpmStartDate,
      grDpmFinishDate,
      grVRProhibitDecisionDate,
      grKOM,
      grKOMMOD,
      grDPM,
      grNGO,
      grRPRF_2699,
      grVRwithAllYearCapacity,
      grVR_2007_2011,
      grVRafter15october,
      grVRNotAllYear,
      grEESupply,
      grPWSupply,
      grPustStation,
      grIsNewGesAes,
      grIsVr,
      grVrFrom,
      grVrTo,
      grKOMMODModernizationStartDate,
      grKOMMODModernizationEndDate,
      grIsKOMMODSelected,
      grKOMMODSupplyStart,
      grIsVRProhibit,
      grIsVyvodSoglasovan,
      grVyvodSoglasovanDate,
      grComment
    } =
    [ grSubject,
      pack grSubjectCode,
      grSubjectFST,
      pack grSubjectFSTCode,
      grStationName,
      pack grStationCode,
      tshow grStationType,
      grGTPGname,
      pack grGTPGCode,
      tMayShow grUnpriceZoneCode,
      tshow grPriceZone,
      tshow grPustGTPG,
      tshow grIsSpotTrader,
      tshow grIsExploitationTypeNormal,
      tshow grIsDPM,
      tshow grRegionRFCode,
      tshow grOES,
      tMayShow grZSP,
      tshow grRGECode,
      tshow grGACode,
      grGaName,
      tMayShow grGEM,
      tshow grGemSelectionResult,
      tshow grVR_KOM,
      tshow grNotSelectedInKOM,
      tshow grKOMRequestAbsent,
      tMayShow grVRNotFromTheYearBegin,
      tMayShow grVRNotUntilTheYearEnd,
      tMayShow grVRNotCondideredInKOM,
      tMayShow grDpmStartDate,
      tMayShow grDpmFinishDate,
      tMayShow grVRProhibitDecisionDate,
      tshow grKOM,
      tshow grKOMMOD,
      tshow grDPM,
      tshow grNGO,
      tshow grRPRF_2699,
      tMayShow grVRwithAllYearCapacity,
      tMayShow grVR_2007_2011,
      tMayShow grVRafter15october,
      tMayShow grVRNotAllYear,
      tshow grEESupply,
      tshow grPWSupply,
      tshow grPustStation,
      tshow grIsNewGesAes,
      tshow grIsVr,
      tMayShow grVrFrom,
      tMayShow grVrTo,
      tMayShow grKOMMODModernizationStartDate,
      tMayShow grKOMMODModernizationEndDate,
      tshow grIsKOMMODSelected,
      tMayShow grKOMMODSupplyStart,
      tMayShow grIsVRProhibit,
      tshow grIsVyvodSoglasovan,
      tMayShow grVyvodSoglasovanDate,
      Text.concat grComment
    ]

generatorEntryHeader :: [Text]
generatorEntryHeader =
  [ "Subject",
    "SubjectCode",
    "SubjectFST",
    "SubjectFSTCode",
    "StationName",
    "StationCode",
    "StationType",
    "GTPGname",
    "GTPGCode",
    "UnpriceZoneCode",
    "PriceZone",
    "PustGTPG",
    "IsSpotTrader",
    "IsExploitationTypeNormal",
    "IsDPM",
    "RegionRFCode",
    "OES",
    "ZSP",
    "RGECode",
    "GACode",
    "GaName",
    "GEM",
    "GemSelectionResult",
    "VR_KOM",
    "NotSelectedInKOM",
    "KOMRequestAbsent",
    "VRNotFromTheYearBegin",
    "VRNotUntilTheYearEnd",
    "VRNotCondideredInKOM",
    "DpmStartDate",
    "DpmFinishDate",
    "VRProhibitDecisionDate",
    "KOM",
    "KOMMOD",
    "DPM",
    "NGO",
    "RPRF_2699",
    "VRwithAllYearCapacity",
    "VR_2007_2011",
    "VRafter15october",
    "VRNotAllYear",
    "EESupply",
    "PWSupply",
    "PustStation",
    "IsNewGesAes",
    "IsVr",
    "VrFrom",
    "VrTo",
    "KOMMODModernizationStartDate",
    "KOMMODModernizationEndDate",
    "IsKOMMODSelected",
    "KOMMODSupplyStart",
    "IsVRProhibit",
    "IsVyvodSoglasovan",
    "VyvodSoglasovanDate",
    "Comment"
  ]

genEntryToCsvFormat :: [GeneratorEntry] -> Text
genEntryToCsvFormat gs =
  let gsText = map (Text.intercalate ";" . generatorEntryToTextList) gs
      gsHeader = Text.intercalate ";" generatorEntryHeader
   in Text.intercalate "\n" (gsHeader : gsText)
