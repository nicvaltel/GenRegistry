{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module Routine (routine) where

import Data.Char (toUpper)
import Data.Either (lefts, partitionEithers)
import Data.List (group, nub, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Debug.Trace (traceShow)
import LoadInputData (InputData (..))
import Types.GeneratorEntry (GeneratorEntry (..))
import Types.KOMTG (KOMTG (..))
import Types.RIOTG (RIOTG (..))
import Types.SoRegistry (SoRegistry (..))
import Types.Types
import Data.Maybe (isNothing, fromMaybe)
import Control.Monad.Writer (Writer, when, MonadWriter (tell))



-- data Regestry a = Regestry {regWarnings :: [Warning], regEntryes :: [a]}

-- instance Semigroup (Regestry a) where
--   reg0 <> reg1 =
--     Regestry {regWarnings = regWarnings reg0 ++ regWarnings reg1 , regEntryes = regEntryes reg0 ++ regEntryes reg1}

-- instance Monoid (Regestry a) where
--   mempty = Regestry{regWarnings = [], regEntryes = []}

-- singletonReg :: a -> Regestry a
-- singletonReg a = Regestry {regWarnings = [], regEntryes = [a]}

routine :: InputData -> Writer [Warning] (Either [ErrorMsg] [GeneratorEntry])
routine inputData@InputData {datRIOTG, datKOMTG, datSoRegistry, datExploitationStartYear, datConstantsAndDates} = do

  tgs <- case filterAndCheckInputData inputData of
    Right tgs -> pure $ Right tgs
    Left err -> pure $ Left [err]
  -- let res = map (`mkPreliminaryGenReg` datKOMTG) tgs

  -- -- getRegPre <- mkPreliminaryGenRegistry tgs2
  


  -- traceShow res $ pure ()
  pure $ Right []

  -- case partitionEithers res of 
  --   ([],ans) -> pure ans
  --   (errs,_) -> Left errs


filterAndCheckInputData :: InputData -> Either ErrorMsg [RIOTG]
filterAndCheckInputData InputData {datRIOTG, datKOMTG, datSoRegistry, datExploitationStartYear, datConstantsAndDates} = do
  checkInputData datRIOTG datKOMTG datSoRegistry
  let pust = getPust datSoRegistry
  let tgs1 = filterRIOTG datRIOTG
  filterRIOTGByPust tgs1 pust datConstantsAndDates

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

filterRIOTGByPust :: [RIOTG] -> Map StationCode Pust -> ConstantsAndDates -> Either ErrorMsg [RIOTG]
filterRIOTGByPust tgs pust ConstantsAndDates{cndMinPust} = do
  let stationNotInPust = [riotgStationCode tg | tg <- tgs, not (Map.member (riotgStationCode tg) pust)]
  _ <- case stationNotInPust of
    [] -> Right []
    _ -> Left $ "filterRIOTGByPustAndDates: some stations from RIOTG not in Pust list: " <> show (nub stationNotInPust)
  pure $ filter (\tg -> pust Map.! riotgStationCode tg >= cndMinPust) tgs

filterRIOTG :: [RIOTG] -> [RIOTG]
filterRIOTG =
  filter ((== EMPTY_XATTR_TYPE) . riotgXAttrType)
    . filter (not . riotgIsGaes)
    . filter (\tg -> (toUpper <$> riotgSubjectCode tg) /= "MOBGTSGK")
    . filter riotgIsExploitationTypeNormal
    . filter riotgIsSpotTrader
    . filter ((\z -> isNothing z || z == Just 0) . riotgIsUnpriceZone)


mkVRParams :: RIOTG -> Writer [Warning] VRParams
mkVRParams rio = 
  if not (riotgIsVR rio)
    then pure NotVR
    else do
      when (isNothing $ riotgVRStartDate rio) $ tell [Warning $ "No start date for VR GTP = " ++ riotgGTPCode rio ++ " GA = " ++ show (riotgGaCode rio)]
      when (isNothing $ riotgVRFinishDate rio) $ tell [Warning $ "No end date for VR GTP = " ++ riotgGTPCode rio ++ " GA = " ++ show (riotgGaCode rio)]
      case (riotgVRStartDate rio, riotgVRFinishDate rio) of
        (Just start, Just finish) -> pure $ ISVR {vrStartDate = start, vrFinishDate = finish}
        _ -> pure NotVR


mkPreliminaryGenReg :: ConstantsAndDates -> RIOTG -> KOMTG -> VRParams -> Writer [Warning] (Either ErrorMsg GeneratorEntry)
mkPreliminaryGenReg ConstantsAndDates{cndStartYearDate, cndFinishYearDate} riotg komtg vrParams = do
  let isVR = case vrParams of ISVR{} -> True; NotVR -> False
  let isDPM = undefined :: Bool
  let isKOMMod = undefined :: Bool
  
  --  TODO check if isVR then riotgVRStartDate and riotgVRFinishDate is Just

  let selectedYearVol = komYearSelectedVolume komtg
  pure $ Right $ GeneratorEntry
        { grSubject = riotgSubject riotg,
          grSubjectCode = riotgSubjectCode riotg,
          grSubjectFST = riotgSubjectFST riotg,
          grSubjectFSTCode = riotgSubjectFSTCode riotg,
          grStationName = riotgStationName riotg,
          grStationCode = riotgStationCode riotg,
          grStationType = riotgStationType riotg,
          grGTPGname = riotgGTPName riotg,
          grGTPGCode = riotgGTPCode riotg,
          grUnpriceZoneCode = riotgIsUnpriceZone riotg,
          grPriceZone = riotgPriceZone riotg,
          grPust = riotgPust riotg,
          grIsSpotTrader = riotgIsSpotTrader riotg,
          grIsExploitationTypeNormal = riotgIsExploitationTypeNormal riotg,
          grIsDPM = riotgIsDPM riotg,
          grRegionRFCode = riotgRegionRFCode riotg,
          grOES = riotgOES riotg,
          grZSP = riotgZSP riotg,
          grRGECode = riotgRgeCode riotg,
          grGACode = riotgGaCode riotg,
          grGaName = riotgGaName riotg,
          grGEM = riotgGEM riotg, -- Принадлежность к ГЕМ
          grGemSelectionResult = -- Результат отбора по ГЕМ
            if isSelectedKom (komtgGemSelectionResult komtg) && selectedYearVol == 0
              then GSRNotSelectedWithZeroYearlyVolume
              else komtgGemSelectionResult komtg              , 
          grVR_KOM = isVR, -- ВР_КОМ
          grNotSelectedInKOM = isSelectedKom (komtgGemSelectionResult komtg) && selectedYearVol > 0, -- Не отобрано
          grKOMRequestAbsent = komtgGemSelectionResult komtg == GSRNotSelectedRefusalToFillRequest, -- Нет_заявки_КОМ
          grVRNotFromTheYearBegin = case vrParams of -- ВР_не_с_начала_года
            NotVR -> Nothing 
            ISVR{vrStartDate} -> Just (vrStartDate > cndStartYearDate), 
          grVRNotUntilTheYearEnd = case vrParams of -- ВР_не_до_конца_года
            NotVR -> Nothing 
            ISVR{vrFinishDate} -> Just (vrFinishDate < cndFinishYearDate), 

          grVRNotCondideredInKOM = if not isVR then Nothing else Just False, -- ВР_не_учтён_в_КОМ
          grDpmStartDate = if not isDPM then Nothing else Just undefined, -- Дата_начала_поставки_ДПМ
          grVRProhibitDecisionDate = if not isVR then Nothing else Just undefined, -- Дата_решения_о_запрете(ВР)
          grKOM = False, -- Группа_КОМ
          grKOMMOD = False, -- Группа_КОММОД
          grDPM = isDPM, -- Группа_ДПМ
          grNGO = False, -- Группа_НГО
          grRPRF_2699 = False, -- Группа_РПРФ2699p
          grVRwithAllYearCapacity = if not isVR then Nothing else Just False, -- Группа_ВР_с_МЩ_весь_год
          grVR_2007_2011 = if not isVR then Nothing else Just False, -- Группа_ВР_2007-2011
          grVRafter15october = if not isVR then Nothing else Just False, -- Группа_ВР_после_15_октября
          grVRNotAllYear = if not isVR then Nothing else Just False, -- Группа_ВР_не_весь_год
          grEESupply = undefined, -- SupplyAttribute, -- Поставка_ЭЭ_по_РД
          grPWSupply = undefined, -- SupplyAttribute, -- Поставка_МЩ_по_РД
          grPustStation = 0, -- Руст_станции
          grIsNewGesAes = False, -- IS_NEW_GES_AES
          grIsVr = isVR, -- IS_VR
          grVrFrom = if not isVR then Nothing else Just undefined, -- Maybe Day, -- ВР c
          grVrTo = if not isVR then Nothing else Just undefined, -- ВР до
          grKOMMODModernizationStartDate = if not isKOMMod then Nothing else Just undefined, -- Maybe Day, -- KOMMOD_START_DATE
          grKOMMODModernizationEndDate = if not isKOMMod then Nothing else Just undefined, -- Maybe Day, -- KOMMOD_END_DATE
          grIsKOMMODSelected = if not isKOMMod then Nothing else Just False, -- KOMMOD_SELECTED
          grKOMMODSupplyStart =  if not isKOMMod then Nothing else Just undefined, -- KOMMOD_SUPPLY_START
          grIsVRProhibit = False, -- IS_VR_ZAPRET
          grIsVyvodSoglasovan = False, -- IS_VUVOD_SOGLASOVAN
          grVyvodSoglasovanDate = if not isKOMMod then Nothing else Just undefined, -- DATA_VUVODA
          grComment = ""
        }
  



-- mkPreliminaryGenReg_back :: ConstantsAndDates -> RIOTG -> [KOMTG] -> Either ErrorMsg ([Warning], GenRegistry)
-- mkPreliminaryGenReg_back ConstantsAndDates{cndStartYearDate} riotg komtgs = do
--   let gaCode = riotgGaCode riotg
--   (warnings, mbKtg) <- case [ktg | ktg <- komtgs, komtgEGOCode ktg == gaCode] of
--     [ktg] -> Right ([], Just ktg)
--     [] -> Right ([Warning $ "mkPreliminaryGenReg: gaCode from RIO does not match to KomByTG (b.t.w. KOM was 5 years ago): gaCode = " <> show gaCode], Nothing)
--     _ -> Left $ "mkPreliminaryGenReg: gaCode from RIO match to several entries in KomByTG: gaCode = " <> show gaCode
--   let isVR = riotgIsVR riotg:: Bool
--   let isDPM = undefined :: Bool
--   let isKOMMod = undefined :: Bool
  
--   --  TODO check if isVR then riotgVRStartDate and riotgVRFinishDate is Just

--   case mbKtg of
--     Just komtg -> 
--       let selectedYearVol = komYearSelectedVolume komtg
--       in Right (warnings, GenRegistry
--         { grSubject = riotgSubject riotg,
--           grSubjectCode = riotgSubjectCode riotg,
--           grSubjectFST = riotgSubjectFST riotg,
--           grSubjectFSTCode = riotgSubjectFSTCode riotg,
--           grStationName = riotgStationName riotg,
--           grStationCode = riotgStationCode riotg,
--           grStationType = riotgStationType riotg,
--           grGTPGname = riotgGTPName riotg,
--           grGTPGCode = riotgGTPCode riotg,
--           grUnpriceZoneCode = riotgIsUnpriceZone riotg,
--           grPriceZone = riotgPriceZone riotg,
--           grPust = riotgPust riotg,
--           grIsSpotTrader = riotgIsSpotTrader riotg,
--           grIsExploitationTypeNormal = riotgIsExploitationTypeNormal riotg,
--           grIsDPM = riotgIsDPM riotg,
--           grRegionRFCode = riotgRegionRFCode riotg,
--           grOES = riotgOES riotg,
--           grZSP = riotgZSP riotg,
--           grRGECode = riotgRgeCode riotg,
--           grGACode = riotgGaCode riotg,
--           grGaName = riotgGaName riotg,
--           grGEM = riotgGEM riotg, -- Принадлежность к ГЕМ
--           grGemSelectionResult = -- Результат отбора по ГЕМ
--             if isSelectedKom (komtgGemSelectionResult komtg) && selectedYearVol == 0
--               then GSRNotSelectedWithZeroYearlyVolume
--               else komtgGemSelectionResult komtg              , 
--           grVR_KOM = isVR, -- ВР_КОМ
--           grNotSelectedInKOM = isSelectedKom (komtgGemSelectionResult komtg) && selectedYearVol > 0, -- Не отобрано
--           grKOMRequestAbsent = komtgGemSelectionResult komtg == GSRNotSelectedRefusalToFillRequest, -- Нет_заявки_КОМ


--           grVRNotFromTheYearBegin = if not isVR then Nothing else Just (riotgVRStartDate riotg > cndStartYearDate), -- ВР_не_с_начала_года
--           grVRNotUntilTheYearEnd = if not isVR then Nothing else Just False, -- ВР_не_до_конца_года
--           grVRNotCondideredInKOM = if not isVR then Nothing else Just False, -- ВР_не_учтён_в_КОМ
--           grDpmStartDate = if not isDPM then Nothing else Just undefined, -- Дата_начала_поставки_ДПМ
--           grVRProhibitDecisionDate = if not isVR then Nothing else Just undefined, -- Дата_решения_о_запрете(ВР)
--           grKOM = False, -- Группа_КОМ
--           grKOMMOD = False, -- Группа_КОММОД
--           grDPM = isDPM, -- Группа_ДПМ
--           grNGO = False, -- Группа_НГО
--           grRPRF_2699 = False, -- Группа_РПРФ2699p
--           grVRwithAllYearCapacity = if not isVR then Nothing else Just False, -- Группа_ВР_с_МЩ_весь_год
--           grVR_2007_2011 = if not isVR then Nothing else Just False, -- Группа_ВР_2007-2011
--           grVRafter15october = if not isVR then Nothing else Just False, -- Группа_ВР_после_15_октября
--           grVRNotAllYear = if not isVR then Nothing else Just False, -- Группа_ВР_не_весь_год
--           grEESupply = undefined, -- SupplyAttribute, -- Поставка_ЭЭ_по_РД
--           grPWSupply = undefined, -- SupplyAttribute, -- Поставка_МЩ_по_РД
--           grPustStation = 0, -- Руст_станции
--           grIsNewGesAes = False, -- IS_NEW_GES_AES
--           grIsVr = isVR, -- IS_VR
--           grVrFrom = if not isVR then Nothing else Just undefined, -- Maybe Day, -- ВР c
--           grVrTo = if not isVR then Nothing else Just undefined, -- ВР до
--           grKOMMODModernizationStartDate = if not isKOMMod then Nothing else Just undefined, -- Maybe Day, -- KOMMOD_START_DATE
--           grKOMMODModernizationEndDate = if not isKOMMod then Nothing else Just undefined, -- Maybe Day, -- KOMMOD_END_DATE
--           grIsKOMMODSelected = if not isKOMMod then Nothing else Just False, -- KOMMOD_SELECTED
--           grKOMMODSupplyStart =  if not isKOMMod then Nothing else Just undefined, -- KOMMOD_SUPPLY_START
--           grIsVRProhibit = False, -- IS_VR_ZAPRET
--           grIsVyvodSoglasovan = False, -- IS_VUVOD_SOGLASOVAN
--           grVyvodSoglasovanDate = if not isKOMMod then Nothing else Just undefined, -- DATA_VUVODA
--           grComment = ""
--         })
--     Nothing -> undefined


isSelectedKom :: GemSelectionResult -> Bool
isSelectedKom = \case
  GSRSelected -> True
  GSRNotSelected -> False
  GSRNotSelectedSupplyAfterMod -> False
  GSRNotSelectedRefusalToFillRequest -> False
  GSRNotSelectedMinimumTechnicalRequirements -> False
  GSRNotSelectedNoTechnicalSupport -> False
  GSRNotSelectedZeroInDecember -> False
  GSRNotSelectedWithZeroYearlyVolume -> False
  GSRMVR -> True
  GSRDPM -> True

komYearSelectedVolume :: KOMTG -> Float
komYearSelectedVolume kom = 
  sum $ map (\maybeVol -> fromMaybe 0 (maybeVol kom))
    [
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
      komtgDec
      ]