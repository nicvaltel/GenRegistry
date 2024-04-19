module Types.GeneratorEntry (GeneratorEntry (..)) where

import Data.Text (Text)
import Data.Time.Calendar (Day)
import Types.Types

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
    grEESupply :: SupplyAttribute, -- Поставка_ЭЭ_по_РД
    grPWSupply :: SupplyAttribute, -- Поставка_МЩ_по_РД
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
    grComment :: String -- Комментарий
  }
  deriving (Show)
