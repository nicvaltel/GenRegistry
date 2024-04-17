{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Routine (routine) where

import Control.Monad.Writer (MonadWriter (tell), Writer, when)
import Data.Char (toUpper)
import Data.List (group, nub, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import LoadInputData (InputData (..))
import Types.ExploitationStartYear (ExploitationStartYear (..))
import Types.GeneratorEntry (GeneratorEntry (..))
import Types.KOMTG (KOMTG (..))
import Types.RIOTG (RIOTG (..))
import Types.SoRegistry (SoRegistry (..))
import Types.Types

routine :: InputData -> Writer [Warning] (Either [ErrorMsg] [GeneratorEntry])
routine inputData@InputData {datRIOTG, datKOMTG, datSoRegistry, datExploitationStartYear, datConstantsAndDates} = do
  -- TODO check that VR in RIO the same as in KOM_PO_TG (МВР /= 0)
  tgs <- case filterAndCheckInputData inputData of
    Right tgs -> pure $ Right tgs
    Left err -> pure $ Left [err]

  pure $ Right []

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
filterRIOTGByPust tgs pust ConstantsAndDates {cndMinPust} = do
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

mkVR2007_2011 :: ConstantsAndDates -> [RIOTG] -> [ExploitationStartYear] -> [GaCode2007_2011]
mkVR2007_2011 ConstantsAndDates {cndYear2007, cndYear2011} riotgs explStartYears =
  let gas_2007_2011 = filter (\y -> y >= cndYear2007 && y <= cndYear2011) $ map esyYear explStartYears
   in filter (`elem` gas_2007_2011) $ map riotgGaCode riotgs

mkVRParams :: RIOTG -> [GaCode2007_2011] -> Writer [Warning] (Maybe VRParams)
mkVRParams rio ga2007_2011 =
  if not (riotgIsVR rio)
    then pure Nothing
    else do
      when (isNothing $ riotgVRStartDate rio) $ tell [Warning $ "No start date for VR GTP = " ++ riotgGTPCode rio ++ " GA = " ++ show (riotgGaCode rio)]
      when (isNothing $ riotgVRFinishDate rio) $ tell [Warning $ "No end date for VR GTP = " ++ riotgGTPCode rio ++ " GA = " ++ show (riotgGaCode rio)]
      when (isNothing $ riotgIsVrZapret rio) $ tell [Warning $ "No zapret status VR GTP = " ++ riotgGTPCode rio ++ " GA = " ++ show (riotgGaCode rio)]
      case (riotgVRStartDate rio, riotgVRFinishDate rio, riotgIsVrZapret rio) of
        (Just start, Just finish, Just zapret) ->
          pure $ Just $ VRParams {vrStartDate = start, vrFinishDate = finish, vrIsVrZapret = zapret, vr2007_2011 = riotgGaCode rio `elem` ga2007_2011}
        _ -> pure Nothing

mkDPMParams :: RIOTG -> Writer [Warning] (Maybe DPMParams)
mkDPMParams rio =
  if not (riotgIsDPM rio)
    then pure Nothing
    else do
      when (isNothing $ riotgDpmStartDate rio) $ tell [Warning $ "No start date for DPM GTP = " ++ riotgGTPCode rio ++ " GA = " ++ show (riotgGaCode rio)]
      when (isNothing $ riotgDpmFinishDate rio) $ tell [Warning $ "No finish date for DPM GTP = " ++ riotgGTPCode rio ++ " GA = " ++ show (riotgGaCode rio)]
      case (riotgDpmStartDate rio, riotgDpmFinishDate rio) of
        (Just start, Just finish) -> pure $ Just $ DPMParams {dpmStartDate = start, dpmFinishDate = finish}
        _ -> pure Nothing

mkKommodParams :: RIOTG -> Writer [Warning] (Maybe KommodParams)
mkKommodParams rio =
  if not (riotgIsKommodSelected rio)
    then pure Nothing
    else do
      when (isNothing $ riotgKommodStartDate rio) $ tell [Warning $ "No start date for KOMMod GTP = " ++ riotgGTPCode rio ++ " GA = " ++ show (riotgGaCode rio)]
      when (isNothing $ riotgKommodFinishDate rio) $ tell [Warning $ "No finish date for KOMMod GTP = " ++ riotgGTPCode rio ++ " GA = " ++ show (riotgGaCode rio)]
      when (isNothing $ riotgKommodSupplyStartDate rio) $ tell [Warning $ "No supply start date for KOMMod GTP = " ++ riotgGTPCode rio ++ " GA = " ++ show (riotgGaCode rio)]
      case (riotgKommodStartDate rio, riotgKommodFinishDate rio, riotgKommodSupplyStartDate rio) of
        (Just start, Just finish, Just supplyDate) -> pure $ Just $ KommodParams {kommodStartDate = start, kommodFinishDate = finish, kommodSupplyStartDate = supplyDate}
        _ -> pure Nothing

mkNGOParams :: RIOTG -> Writer [Warning] (Maybe NGOParams)
mkNGOParams rio =
  if not (riotgIsKomNgoSelected rio)
    then pure Nothing
    else do
      when (isNothing $ riotgNgoStartDate rio) $ tell [Warning $ "No start date for NGO GTP = " ++ riotgGTPCode rio ++ " GA = " ++ show (riotgGaCode rio)]
      when (isNothing $ riotgNgoFinishDate rio) $ tell [Warning $ "No finish date for NGO GTP = " ++ riotgGTPCode rio ++ " GA = " ++ show (riotgGaCode rio)]
      case (riotgNgoStartDate rio, riotgNgoFinishDate rio) of
        (Just start, Just finish) -> pure $ Just $ NGOParams {ngoStartDate = start, ngoFinishDate = finish}
        _ -> pure Nothing

mkRPRF2699Params :: RIOTG -> Writer [Warning] (Maybe RPRF2699Params)
mkRPRF2699Params rio =
  if not (riotgIsKomNgoSelected rio)
    then pure Nothing
    else do
      when (isNothing $ riotgRprf2699StartDate rio) $ tell [Warning $ "No start date for RPRF2699 GTP = " ++ riotgGTPCode rio ++ " GA = " ++ show (riotgGaCode rio)]
      pure $ (\start -> RPRF2699Params {rprf2699StartDate = start}) <$> riotgRprf2699StartDate rio

mkVyvodSoglasovan :: RIOTG -> Writer [Warning] (Maybe VyvodSoglasovan)
mkVyvodSoglasovan rio =
  if not (riotgIsVyvodSoglasovan rio)
    then pure Nothing
    else do
      when (isNothing $ riotgVyvodDate rio) $ tell [Warning $ "No soglasovanniy vyvod date for GTP = " ++ riotgGTPCode rio ++ " GA = " ++ show (riotgGaCode rio)]
      pure $ (\start -> VyvodSoglasovan {vyvodSoglasovanDate = start}) <$> riotgVyvodDate rio

mkGeneratorEntry ::
  ConstantsAndDates ->
  RIOTG ->
  KOMTG ->
  SoRegistry ->
  Maybe VRParams ->
  Maybe DPMParams ->
  Maybe KommodParams ->
  Maybe NGOParams ->
  Maybe RPRF2699Params ->
  Maybe VyvodSoglasovan ->
  Writer [Warning] (Either ErrorMsg GeneratorEntry)
mkGeneratorEntry ConstantsAndDates {cndStartYearDate, cndFinishYearDate, cndVrBanDate} riotg komtg soReg vr dpm kommod ngo rprf2699 vyvodSoglasovan = do
  let isVR = isJust vr
  let isDPM = isJust dpm
  let isKOMMod = isJust kommod

  let isKOM -- TODO update by query  -- Группа_КОМ
        =
        isSelectedKom (komtgGemSelectionResult komtg)
          || and
            [ komtgGemSelectionResult komtg == GSRDPM,
              isNothing dpm,
              not $ riotgIsNewGesAes riotg,
              isNothing kommod
            ]

  let vrKOM = komtgGemSelectionResult komtg == GSRMVR -- ВР_КОМ
  let vrProhibitDecisionDate = case vr of -- Дата_решения_о_запрете(ВР)
        Just v -> if vrIsVrZapret v then Just (vrStartDate v) else Nothing
        Nothing -> Nothing

  -- Группа_ВР_с_МЩ_весь_год:
  -- IIf (IsNull ([Дата_решения_о_запрете(ВР)]);
  --         IIf ([ВР_КОМ]=1 And [ВР_2007_2011]=0;1;0);
  --         IIf (([ВР_КОМ]=1 Or (([Дата_решения_о_запрете(ВР)])<=[дата_запрета_на_ВР])) And [ВР_2007_2011]=0;1;0)
  --     )
  let vrWithAllYearCapacity -- Группа_ВР_с_МЩ_весь_год
        =
        case vr of
          Nothing -> Nothing
          Just vrparams -> case vrProhibitDecisionDate of
            Nothing -> Just (vrKOM && not (vr2007_2011 vrparams)) -- IIf([ВР_КОМ]=1 And [ВР_2007_2011]=0;1;0)
            Just prohibitDecisionDay -> Just $ vrKOM || (prohibitDecisionDay <= cndVrBanDate && not (vr2007_2011 vrparams)) -- IIf(([ВР_КОМ]=1 Or (([Дата_решения_о_запрете(ВР)])<=[дата_запрета_на_ВР])) And [ВР_2007_2011]=0;1;0)
  let selectedYearVol = komYearSelectedVolume komtg
  pure $
    Right $
      GeneratorEntry
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
          grGemSelectionResult -- Результат отбора по ГЕМ
          =
            if isSelectedKom (komtgGemSelectionResult komtg) && selectedYearVol == 0
              then GSRNotSelectedWithZeroYearlyVolume
              else komtgGemSelectionResult komtg,
          grVR_KOM = vrKOM, -- ВР_КОМ
          grNotSelectedInKOM = isSelectedKom (komtgGemSelectionResult komtg) && selectedYearVol > 0, -- Не отобрано
          grKOMRequestAbsent = komtgGemSelectionResult komtg == GSRNotSelectedRefusalToFillRequest, -- Нет_заявки_КОМ
          grVRNotFromTheYearBegin = (\v -> vrStartDate v > cndStartYearDate) <$> vr,
          grVRNotUntilTheYearEnd = (\v -> vrFinishDate v < cndFinishYearDate) <$> vr,
          grVRNotCondideredInKOM = (\_ -> komtgGemSelectionResult komtg /= GSRMVR) <$> vr, -- ВР_не_учтён_в_КОМ
          grDpmStartDate = dpmStartDate <$> dpm, -- Дата_начала_поставки_ДПМ
          grVRProhibitDecisionDate = vrProhibitDecisionDate, -- Дата_решения_о_запрете(ВР)
          grKOM = isKOM, -- Группа_КОМ
          grKOMMOD = maybe False (\k -> kommodSupplyStartDate k <= cndFinishYearDate) kommod,
          grDPM = isJust dpm, -- Группа_ДПМ
          grNGO = maybe False (\n -> ngoStartDate n <= cndStartYearDate) ngo, -- Группа_НГО
          grRPRF_2699 = maybe False (\r -> rprf2699StartDate r <= cndStartYearDate) rprf2699, -- Группа_РПРФ2699p
          grVRwithAllYearCapacity = vrWithAllYearCapacity, -- Группа_ВР_с_МЩ_весь_год
          grVR_2007_2011 = vr2007_2011 <$> vr, -- Группа_ВР_2007-2011
          grVRafter15october = case vr of -- Группа_ВР_после_15_октября
            Nothing -> Nothing
            Just _ -> Just $ maybe False (>= cndVrBanDate) vrProhibitDecisionDate,
          grVRNotAllYear = (\v -> vrFinishDate v < cndFinishYearDate || vrStartDate v > cndStartYearDate) <$> vr, -- Группа_ВР_не_весь_год
          grEESupply = SupplyAllYear, -- SupplyAttribute, -- Поставка_ЭЭ_по_РД
          grPWSupply -- SupplyAttribute, -- Поставка_МЩ_по_РД: IIf(([Группа_КОМ]=1 Or [Группа_КОММОД]=1 Or [Группа_ВР_с_МЩ_весь_год]=1) And [Группа_РПРФ2699p]<>1;1;0)
          =
            if isNothing rprf2699 && (isKOM || isJust kommod || fromMaybe False vrWithAllYearCapacity)
              then SupplyAllYear
              else NoSupply,
          grPustStation = sorUstPower soReg, -- Руст_станции
          grIsNewGesAes = riotgIsNewGesAes riotg, -- IS_NEW_GES_AES
          grIsVr = isVR, -- IS_VR
          grVrFrom = vrStartDate <$> vr, -- Maybe Day, -- ВР c
          grVrTo = vrFinishDate <$> vr, -- ВР до
          grKOMMODModernizationStartDate = kommodStartDate <$> kommod, -- Maybe Day, -- KOMMOD_START_DATE
          grKOMMODModernizationEndDate = kommodFinishDate <$> kommod, -- Maybe Day, -- KOMMOD_END_DATE
          grIsKOMMODSelected = isJust kommod, -- KOMMOD_SELECTED
          grKOMMODSupplyStart = kommodSupplyStartDate <$> kommod, -- KOMMOD_SUPPLY_START
          grIsVRProhibit = vrIsVrZapret <$> vr, -- IS_VR_ZAPRET
          grIsVyvodSoglasovan = isJust vyvodSoglasovan, -- IS_VUVOD_SOGLASOVAN
          grVyvodSoglasovanDate = vyvodSoglasovanDate <$> vyvodSoglasovan, -- DATA_VUVODA
          grComment = ""
        }

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
  sum $
    map
      (\maybeVol -> fromMaybe 0 (maybeVol kom))
      [ komtgJan,
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