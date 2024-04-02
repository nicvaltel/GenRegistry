{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Routine where

import Data.Char (toUpper)
import Data.Either (lefts)
import Data.List (group, nub, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Debug.Trace (traceShow)
import LoadInputData (InputData (..))
import Types.GenRegistry (GenRegistry (..))
import Types.KOMTG (KOMTG (..))
import Types.RIOTG (RIOTG (..))
import Types.SoRegistry (SoRegistry (..))
import Types.Types

type ErrorMsg = String

routine :: InputData -> Either [ErrorMsg] [GenRegistry]
routine inputData@InputData {datRIOTG, datKOMTG, datSoRegistry, datExploitationStartYear, datYearDate} = do
  -- tgs <- case filterAndCheckInputData inputData of
  --   Right tgs -> Right tgs
  --   Left err -> Left [err]
  -- let res = map (flip mkPreliminaryGenReg datKOMTG) tgs
  -- -- getRegPre <- mkPreliminaryGenRegistry tgs2

  -- traceShow res $ pure ()
  -- -- pure []
  -- res

  undefined

filterAndCheckInputData :: InputData -> Either ErrorMsg [RIOTG]
filterAndCheckInputData InputData {datRIOTG, datKOMTG, datSoRegistry, datExploitationStartYear, datYearDate} = do
  checkInputData datRIOTG datKOMTG datSoRegistry
  let pust = getPust datSoRegistry
  let tgs1 = filterRIOTG datRIOTG
  filterRIOTGByPust tgs1 pust datYearDate

checkInputData :: [RIOTG] -> [KOMTG] -> [SoRegistry] -> Either ErrorMsg ()
checkInputData riotg komtg soReg = do
  case filter ((/= 1) . length) . group . sort $ map riotgGaCode riotg of
    [] -> Right ()
    doubles -> Left $ "Not unique GaCode in RIO_TG: " ++ show (nub . concat $ doubles)
  case filter ((/= 1) . length) . group . sort $ map komtgEGOCode komtg of
    [] -> Right ()
    doubles -> Left $ "Not unique EGOCode in KOM_TG: " ++ show (nub . concat $ doubles)
  case filter ((/= 1) . length) . group . sort $ sorGtpCode <$> soReg of
    [] -> Right ()
    doubles -> Left $ "Not unique GTPG in SO Registry: " ++ show (nub . concat $ doubles)

getPust :: [SoRegistry] -> Map StationCode Pust
getPust = foldr (\so acc -> Map.insertWith (+) (sorStationCode so) (sorUstPower so) acc) Map.empty

filterRIOTGByPust :: [RIOTG] -> Map StationCode Pust -> YearDate -> Either ErrorMsg [RIOTG]
filterRIOTGByPust tgs pust YearDate {ydMinPust} = do
  let stationNotInPust = [riotgStationCode tg | tg <- tgs, not (Map.member (riotgStationCode tg) pust)]
  _ <- case stationNotInPust of
    [] -> Right []
    _ -> Left $ "filterRIOTGByPustAndDates: some stations from RIOTG not in Pust list: " <> show (nub stationNotInPust)
  Right $ filter (\tg -> pust Map.! riotgStationCode tg >= ydMinPust) tgs

filterRIOTG :: [RIOTG] -> [RIOTG]
filterRIOTG =
  filter ((== EMPTY_XATTR_TYPE) . riotgXAttrType)
    . filter (not . riotgIsGaes)
    . filter (\tg -> (toUpper <$> riotgSubjectCode tg) /= "MOBGTSGK")
    . filter riotgIsExploitationTypeNormal
    . filter riotgIsSpotTrader
    . filter ((== 0) . riotgIsUnpriceZone)

mkPreliminaryGenReg :: RIOTG -> [KOMTG] -> Either ErrorMsg GenRegistry
mkPreliminaryGenReg riotg komtgs = do
  let gaCode = riotgGaCode riotg
  komtg <- case [ktg | ktg <- komtgs, komtgEGOCode ktg == gaCode] of
    [ktg] -> Right ktg
    _ -> Left $ "mkPreliminaryGenReg: gaCode from RIO does not match to KomByTG: gaCode = " <> show gaCode
  Right $ undefined

-- GenRegistry
-- { grSubject = riotgSubject riotg,
--   grSubjectCode = riotgSubjectCode riotg,
--   grSubjectFST = riotgSubjectFST riotg,
--   grSubjectFSTCode = riotgSubjectFSTCode riotg,
--   grStationName = riotgStationName riotg,
--   grStationCode = riotgStationCode riotg,
--   grStationType = riotgStationType riotg,
--   grGTPGname = riotgGTPName riotg,
--   grGTPGCode = riotgGTPCode riotg,
--   grIsUnpriceZone = riotgIsUnpriceZone riotg,
--   grPriceZone = riotgPriceZone riotg,
--   grPust = riotgPust riotg,
--   grIsSpotTrader = riotgIsSpotTrader riotg,
--   grIsExploitationTypeNormal = riotgIsExploitationTypeNormal riotg,
--   grIsDPM = riotgIsDPM riotg,
--   grRegionRFCode = riotgRegionRFCode riotg,
--   grOES = riotgOES riotg,
--   grZSP = riotgZSP riotg,
--   grRGECode = riotgRgeCode riotg,
--   grGACode = riotgGaCode riotg,
--   grGaName = riotgGaName riotg,
--   grGEM = riotgGEM riotg, -- Принадлежность к ГЕМ
--   grGemSelectionResult = GemSelectionResult, -- Результат отбора по ГЕМ
--   grVR_KOM = Maybe Bool, -- ВР_КОМ
--   grNotSelectedInKOM = Bool, -- Не отобрано
--   grKOMRequestAbsent = Bool, -- Нет_заявки_КОМ
--   grVRNotFromTheYearBegin = Maybe Bool, -- ВР_не_с_начала_года
--   grVRNotUntilTheYearEnd = Maybe Bool, -- ВР_не_до_конца_года
--   grVRNotCondideredInKOM = Maybe Bool, -- ВР_не_учтён_в_КОМ
--   grDpmStartDate = Maybe Day, -- Дата_начала_поставки_ДПМ
--   grVRProhibitDecisionDate = Maybe Day, -- Дата_решения_о_запрете(ВР)
--   grKOM = Bool, -- Группа_КОМ
--   grKOMMOD = Bool, -- Группа_КОММОД
--   grDPM = Bool, -- Группа_ДПМ
--   grNGO = Bool, -- Группа_НГО
--   grRPRF_2699 = Bool, -- Группа_РПРФ2699p
--   grVRwithAllYearCapacity = Maybe Bool, -- Группа_ВР_с_МЩ_весь_год
--   grVR_2007_2011 = Maybe Bool, -- Группа_ВР_2007-2011
--   grVRafter15october = Maybe Bool, -- Группа_ВР_после_15_октября
--   grVRNotAllYear = Maybe Bool, -- Группа_ВР_не_весь_год
--   grEESupply = SupplyAttribute, -- Поставка_ЭЭ_по_РД
--   grPWSupply = SupplyAttribute, -- Поставка_МЩ_по_РД
--   grPustStation = Pust, -- Руст_станции
--   grIsNewGesAes = Bool, -- IS_NEW_GES_AES
--   grIsVr = Bool, -- IS_VR
--   grVrFrom = Maybe Day, -- ВР c
--   grVrTo = Maybe Day, -- ВР до
--   grKOMMODModernizationStartDate = Maybe Day, -- KOMMOD_START_DATE
--   grKOMMODModernizationEndDate = Maybe Day, -- KOMMOD_END_DATE
--   grIsKOMMODSelected = Maybe Bool, -- KOMMOD_SELECTED
--   grKOMMODSupplyStart = Maybe Day, -- KOMMOD_SUPPLY_START
--   grIsVRProhibit = Bool, -- IS_VR_ZAPRET
--   grIsVyvodSoglasovan = Bool, -- IS_VUVOD_SOGLASOVAN
--   grVyvodSoglasovanDate = Maybe Day, -- DATA_VUVODA
--   grComment = ""
-- }