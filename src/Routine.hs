{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Routine (routine) where

import Control.Monad (forM, forM_)
import Control.Monad.Writer (MonadWriter (tell), Writer, when)
import Data.Char (toUpper)
import Data.List (group, nub, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Time.Calendar (Day)
import LoadInputData (InputData (..))
import Types.ExploitationStartYear (ExploitationStartYear (..))
import Types.GeneratorEntry (GeneratorEntry (..))
import Types.KOMTG (KOMTG (..))
import Types.RIOTG (RIOTG (..))
import Types.SoRegistry (SoRegistry (..))
import Types.Types
import Utils (countElems)

routine :: InputData -> Writer [Warning] [GeneratorEntry]
routine inputData@InputData {datRIOTG, datKOMTG, datSoRegistry, datExploitationStartYear, datConstantsAndDates} = do
  -- TODO check that VR in RIO the same as in KOM_PO_TG (МВР /= 0)
  -- TODO check that Pust in RIO the same as in SO
  -- TODO unique EGOCode in datKOMTG
  -- TODO change cnd to Reader
  -- TODO change SupplyPeriod to [SupplyPeriod] and intersect them at the end
  -- TODO change SupplyPeriod to SupplyPeriod 'EE and 'PW

  (pustStations, pustGTPGs) <- getPust datRIOTG datSoRegistry
  let tgs = either error id (filterAndCheckInputData inputData pustStations)
  let vr2007_2011 = mkVR2007_2011 datConstantsAndDates datRIOTG datExploitationStartYear
  let komMap = Map.fromList $ map (\k -> (komtgEGOCode k, k)) datKOMTG

  gs <- forM tgs $ \rio -> do
      let mbkom = Map.lookup (riotgGaCode rio) komMap
      let pustSt = fromMaybe (error $ "No Pust for Station = " ++ riotgStationCode rio) (Map.lookup (riotgStationCode rio) pustStations)
      let pustGTPG = fromMaybe (error $ "No Pust for GTPG = " ++ riotgGTPCode rio) (Map.lookup (riotgGTPCode rio) pustGTPGs)
      vrParams <- mkVRParams rio vr2007_2011
      dpmParams <- mkDPMParams rio
      kommodParams <- mkKommodParams (cndStartYearDate datConstantsAndDates) rio
      ngoParams <- mkNGOParams rio
      rprf2699Params <- mkRPRF2699Params rio
      vyvodSoglasovan <- mkVyvodSoglasovan rio
      gePrelim <- mkGeneratorEntryPreliminarily datConstantsAndDates rio mbkom pustSt pustGTPG vrParams dpmParams kommodParams ngoParams rprf2699Params vyvodSoglasovan
      pure $ updateGeneratorEntry datConstantsAndDates gePrelim mbkom vrParams kommodParams
  pure $ concatMap (doubleDPM datConstantsAndDates) gs


filterAndCheckInputData :: InputData -> Map StationCode Pust -> Either ErrorMsg [RIOTG]
filterAndCheckInputData InputData {datRIOTG, datKOMTG, datSoRegistry, datExploitationStartYear = _, datConstantsAndDates} pustStations = do
  checkInputData datRIOTG datKOMTG datSoRegistry
  let tgs1 = filterRIOTG datRIOTG
  filterRIOTGByPust tgs1 pustStations datConstantsAndDates

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

getPust :: [RIOTG] -> [SoRegistry] -> Writer [Warning] (Map StationCode Pust, Map GTPGCode Pust)
getPust rios soregs = do
  let rioPustByGTPG = nub $ map (\r -> (riotgGTPCode r, riotgStationCode r, riotgPust r, riotgIsSpotTrader r)) rios
  let notUniqueGTPG = filter (\(_, n) -> n > 1) (countElems $ map (\(gtpg, _, _, _) -> gtpg) rioPustByGTPG)
  -- check for unique Pust for every GTPG in RIO
  forM_ notUniqueGTPG $ \(gtpg, _) ->
    tell [Warning $ "Several different Pust or StationCode for the same GTPG is RIO. GTP = " ++ gtpg]
  pustList <- forM rioPustByGTPG $ \(gtpgCode, stCode, pustRIO, isSpotTrader) -> do
    let soreg = filter (\s -> sorGtpCode s == gtpgCode) soregs
    case soreg of
      [] -> do
        when isSpotTrader $ tell [Warning $ "No GTPG is SO Regestry. GTP = " ++ gtpgCode]
        pure (stCode, gtpgCode, pustRIO)
      [so] -> pure (stCode, gtpgCode, sorUstPower so)
      _ -> do
        tell [Warning $ "Several GTPGs entryes is SO Regestry. GTP = " ++ gtpgCode]
        pure (stCode, gtpgCode, pustRIO)
  let gptgPust = Map.fromList $ map (\(_, g, p) -> (g, p)) pustList

  let stationPustList = map (\(s, _, p) -> (s, p)) pustList
  let stationPust = foldr (\(stCode, pust) acc -> Map.insertWith (+) stCode pust acc) Map.empty stationPustList
  pure (stationPust, gptgPust)

mkVR2007_2011 :: ConstantsAndDates -> [RIOTG] -> [ExploitationStartYear] -> Set.Set GaCode2007_2011
mkVR2007_2011 ConstantsAndDates {cndYear2007, cndYear2011} riotgs explStartYears =
  let gas_2007_2011 = filter (\y -> y >= cndYear2007 && y <= cndYear2011) $ map esyYear explStartYears
   in Set.fromList $ filter (`elem` gas_2007_2011) $ map riotgGaCode riotgs

mkVRParams :: RIOTG -> Set.Set GaCode2007_2011 -> Writer [Warning] (Maybe VRParams)
mkVRParams rio ga2007_2011 =
  if not (riotgIsVR rio)
    then pure Nothing
    else do
      when (isNothing $ riotgVRStartDate rio) $ tell [Warning $ "No start date for VR GTP = " ++ riotgGTPCode rio ++ " GA = " ++ show (riotgGaCode rio)]
      when (isNothing $ riotgVRFinishDate rio) $ tell [Warning $ "No end date for VR GTP = " ++ riotgGTPCode rio ++ " GA = " ++ show (riotgGaCode rio)]
      -- when (isNothing $ riotgIsVrZapret rio) $ tell [Warning $ "No zapret status VR GTP = " ++ riotgGTPCode rio ++ " GA = " ++ show (riotgGaCode rio)]
      case (riotgVRStartDate rio, riotgVRFinishDate rio) of
        (Just start, Just finish) ->
          pure $ Just $ VRParams {vrStartDate = start, vrFinishDate = finish, vrIsVrZapret = fromMaybe False (riotgIsVrZapret rio), vr2007_2011 = riotgGaCode rio `elem` ga2007_2011}
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

mkKommodParams :: Day -> RIOTG -> Writer [Warning] (Maybe KommodParams)
mkKommodParams startYearDate rio =
  if not (riotgIsKommodSelected rio)
    then pure Nothing
    else do
      let kommodSupplyIsNotStartedYet = maybe True (> startYearDate) (riotgKommodSupplyStartDate rio)
      when (isNothing (riotgKommodStartDate rio) && kommodSupplyIsNotStartedYet) $
        tell [Warning $ "No start date for KOMMod GTP = " ++ riotgGTPCode rio ++ " GA = " ++ show (riotgGaCode rio) ++ (show (riotgKommodSupplyStartDate rio) ++ " " ++ show startYearDate)]
      when (isNothing (riotgKommodFinishDate rio) && kommodSupplyIsNotStartedYet) $ tell [Warning $ "No finish date for KOMMod GTP = " ++ riotgGTPCode rio ++ " GA = " ++ show (riotgGaCode rio)]
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
  if not (riotgIsRprf2699 rio)
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

mkGeneratorEntryPreliminarily ::
  ConstantsAndDates ->
  RIOTG ->
  Maybe KOMTG ->
  Pust ->
  Pust ->
  Maybe VRParams ->
  Maybe DPMParams ->
  Maybe KommodParams ->
  Maybe NGOParams ->
  Maybe RPRF2699Params ->
  Maybe VyvodSoglasovan ->
  Writer [Warning] GeneratorEntry
mkGeneratorEntryPreliminarily ConstantsAndDates {cndStartYearDate, cndFinishYearDate, cndVrBanDate} riotg mbkom pustStation pustGTPG vr dpm kommod ngo rprf2699 vyvodSoglasovan = do
  let isKOM -- TODO update by query  -- Группа_КОМ
        =
        case mbkom of
          Nothing -> False
          Just komtg ->
            isSelectedKom (komtgGemSelectionResult komtg)
              || and
                [ komtgGemSelectionResult komtg == GSRDPM,
                  isNothing dpm,
                  not $ riotgIsNewGesAes riotg,
                  isNothing kommod
                ]

  let vrKOM = maybe False (\kom -> komtgGemSelectionResult kom == GSRMVR) mbkom -- ВР_КОМ
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
  let selectedYearVol = maybe 0 komYearSelectedVolume mbkom
  pure
    GeneratorEntry
      { grSubject = riotgSubject riotg, -- Участник
        grSubjectCode = riotgSubjectCode riotg, -- Код участника
        grSubjectFST = riotgSubjectFST riotg, -- Участник ФСТ
        grSubjectFSTCode = riotgSubjectFSTCode riotg, -- Код участника ФСТ
        grStationName = riotgStationName riotg, -- Станция
        grStationCode = riotgStationCode riotg, -- Код станции
        grStationType = riotgStationType riotg, -- Тип станции
        grGTPGname = riotgGTPName riotg, -- ГТП
        grGTPGCode = riotgGTPCode riotg, -- Код ГТП
        grUnpriceZoneCode = riotgIsUnpriceZone riotg, -- Признак неценовой зоны
        grPriceZone = riotgPriceZone riotg, -- Ценовая зона
        grPustGTPG = pustGTPG, -- riotgPust riotg, -- установленная мощность ГТПГ
        grIsSpotTrader = riotgIsSpotTrader riotg, -- выход_на_спот
        grIsExploitationTypeNormal = riotgIsExploitationTypeNormal riotg, -- характер_экспл
        grIsDPM = riotgIsDPM riotg, -- IS_DPM
        grRegionRFCode = riotgRegionRFCode riotg, -- Регион_РФ
        grOES = riotgOES riotg, -- ОЭС
        grZSP = riotgZSP riotg, -- ЗСП
        grRGECode = riotgRgeCode riotg, -- Код РГЕ
        grGACode = riotgGaCode riotg, -- Код ГА
        grGaName = riotgGaName riotg, -- ГА
        grGEM = riotgGEM riotg, -- Принадлежность к ГЕМ
        grGemSelectionResult -- Результат отбора по ГЕМ
        =
          maybe
            GSRisNull
            ( \kom ->
                if isSelectedKom (komtgGemSelectionResult kom) && selectedYearVol == 0
                  then GSRNotSelectedWithZeroYearlyVolume
                  else komtgGemSelectionResult kom
            )
            mbkom,
        grVR_KOM = vrKOM, -- ВР_КОМ
        grNotSelectedInKOM = maybe True (\kom -> not (isSelectedKom (komtgGemSelectionResult kom) && selectedYearVol > 0)) mbkom, -- Не отобрано
        grKOMRequestAbsent = maybe False (\kom -> komtgGemSelectionResult kom == GSRNotSelectedRefusalToFillRequest) mbkom, -- Нет_заявки_КОМ
        grVRNotFromTheYearBegin = (\v -> vrStartDate v > cndStartYearDate) <$> vr,
        grVRNotUntilTheYearEnd = (\v -> vrFinishDate v < cndFinishYearDate) <$> vr,
        grVRNotCondideredInKOM = (\_ -> maybe True (\kom -> komtgGemSelectionResult kom /= GSRMVR) mbkom) <$> vr, -- ВР_не_учтён_в_КОМ
        grDpmStartDate = dpmStartDate <$> dpm, -- Дата_начала_поставки_ДПМ
        grDpmFinishDate = dpmFinishDate <$> dpm, -- Дата окончания поставки ДПМ (новое поле)
        grVRProhibitDecisionDate = vrProhibitDecisionDate, -- Дата_решения_о_запрете(ВР)
        grKOM = isKOM, -- Группа_КОМ
        grKOMMOD = maybe False (\k -> kommodSupplyStartDate k <= cndFinishYearDate) kommod,
        grDPM = isJust dpm || riotgIsNewGesAes riotg, -- Группа_ДПМ
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
        grPustStation = pustStation, -- Руст_станции
        grIsNewGesAes = riotgIsNewGesAes riotg, -- IS_NEW_GES_AES
        grIsVr = isJust vr, -- IS_VR
        grVrFrom = vrStartDate <$> vr, -- Maybe Day, -- ВР c
        grVrTo = vrFinishDate <$> vr, -- ВР до
        grKOMMODModernizationStartDate = kommodStartDate <$> kommod, -- Maybe Day, -- KOMMOD_START_DATE
        grKOMMODModernizationEndDate = kommodFinishDate <$> kommod, -- Maybe Day, -- KOMMOD_END_DATE
        grIsKOMMODSelected = isJust kommod, -- KOMMOD_SELECTED
        grKOMMODSupplyStart = kommodSupplyStartDate <$> kommod, -- KOMMOD_SUPPLY_START
        grIsVRProhibit = vrIsVrZapret <$> vr, -- IS_VR_ZAPRET
        grIsVyvodSoglasovan = isJust vyvodSoglasovan, -- IS_VUVOD_SOGLASOVAN
        grVyvodSoglasovanDate = vyvodSoglasovanDate <$> vyvodSoglasovan, -- DATA_VUVODA
        grComment = []
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
  GSRKommod -> True
  GSRKomNgo -> True
  GSRSelectedRPRF2699 -> True
  GSRisNull -> False

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

updateGeneratorEntry :: ConstantsAndDates -> GeneratorEntry -> Maybe KOMTG -> Maybe VRParams -> Maybe KommodParams -> GeneratorEntry
updateGeneratorEntry cnd gr mbkom mbvr mbkommod =
  let startYear = cndStartYearDate cnd
      endYear = cndFinishYearDate cnd
      updateFunctions =
        [ updateWhen grKOMRequestAbsent (\g -> g {grNotSelectedInKOM = True}),
          updateWhen
            (\g -> (grGemSelectionResult g == GSRSelected) && maybe True (\k -> komYearSelectedVolume k == 0) mbkom)
            (\g -> g {grKOM = False, grNotSelectedInKOM = True}),
          updateWhen grKOMMOD (\g -> g {grGemSelectionResult = GSRKommod}),
          updateWhen grNGO (\g -> g {grGemSelectionResult = GSRKomNgo, grComment = grComment g ++ ["Договор КОМНГО, аналогичный ДПМ"]}),
          updateWhen grRPRF_2699 (\g -> g {grGemSelectionResult = GSRSelectedRPRF2699, grComment = grComment g ++ ["Секретные Крымские. РД по мощности им не делали. 'Отобраны в КОМ' по распоряжению правительства 26-99р"]}),
          updateWhen (\_ -> maybe False vr2007_2011 mbvr) (\g -> g {grVR_2007_2011 = Just True}),
          \g -> case mbkommod of
            Nothing -> g
            Just KommodParams {kommodStartDate}
              | kommodStartDate <= endYear ->
                  g
                    { grComment = grComment g ++ ["Модернизируется на КОММОД с " <> Text.pack (show kommodStartDate)],
                      grPWSupply = supplyAttributeIntersection (grPWSupply g) SupplyPeriod {supplyPeriodFrom = startYear, supplyPeriodTo = kommodStartDate}
                    }
              | otherwise -> g,
          \g -> case mbkommod of
            Nothing -> g
            Just KommodParams {kommodFinishDate}
              | kommodFinishDate < endYear ->
                  g
                    { grComment = grComment g ++ ["Модернизируется до " <> Text.pack (show kommodFinishDate) <> ", затем уходит с Опта"],
                      grEESupply = supplyAttributeIntersection (grEESupply g) SupplyPeriod {supplyPeriodFrom = startYear, supplyPeriodTo = kommodFinishDate}
                    }
              | otherwise -> g,
          \g -> case mbkommod of
            Nothing -> g
            Just KommodParams {kommodSupplyStartDate} ->
              g
                { grComment = grComment g ++ ["Поставка после модернизаци по договору КОММОД с " <> Text.pack (show kommodSupplyStartDate)],
                  grPWSupply = case grGemSelectionResult g of
                    GSRisNull ->
                      if kommodSupplyStartDate <= cndFirstApril cnd
                        then SupplyPeriod {supplyPeriodFrom = cndSecondTermDate cnd, supplyPeriodTo = cndFinishYearDate cnd}
                        else NoSupply
                    _ -> grPWSupply g
                },
          \g -> case grVyvodSoglasovanDate g of
            Nothing -> g
            Just dataVyvoda
              | dataVyvoda >= cndFinishYearDate cnd -> g
              | otherwise ->
                  g
                    { grComment = grComment g ++ ["Согласован вывод из эксплуатаци с " <> Text.pack (show dataVyvoda)],
                      grPWSupply =
                        if dataVyvoda <= cndStartYearDate cnd
                          then NoSupply
                          else
                            if dataVyvoda < cndFinishYearDate cnd
                              then supplyAttributeIntersection (grPWSupply g) SupplyPeriod {supplyPeriodFrom = cndStartYearDate cnd, supplyPeriodTo = dataVyvoda}
                              else grPWSupply g,
                      grEESupply =
                        if dataVyvoda <= cndStartYearDate cnd
                          then NoSupply
                          else
                            if dataVyvoda < cndFinishYearDate cnd
                              then supplyAttributeIntersection (grEESupply g) SupplyPeriod {supplyPeriodFrom = cndStartYearDate cnd, supplyPeriodTo = dataVyvoda}
                              else grEESupply g
                    }
        ]
   in foldr (\upFunc acc -> upFunc acc) gr updateFunctions
  where
    updateWhen :: (a -> Bool) -> (a -> a) -> a -> a
    updateWhen predicate upFunc x = if predicate x then upFunc x else x

doubleDPM :: ConstantsAndDates -> GeneratorEntry -> [GeneratorEntry]
doubleDPM cnd gr =
  let startYear = cndStartYearDate cnd
      endYear = cndFinishYearDate cnd
   in case grDpmFinishDate gr of
        Nothing -> [gr]
        Just finish ->
          if (grIsDPM gr || grIsNewGesAes gr) && (finish >= startYear && finish < endYear)
            then
              [ gr
                  { grIsDPM = False,
                    grDPM = False,
                    grPWSupply = SupplyPeriod {supplyPeriodFrom = finish, supplyPeriodTo = endYear},
                    grComment = grComment gr ++ ["Без ДПМная часть - ДПМ не до конца года - Дата прекращения ДПМ: " <> Text.pack (show finish)]
                  },
                gr
                  { grIsDPM = True,
                    grDPM = True,
                    grKOM = False,
                    grPWSupply = NoSupply,
                    grComment = grComment gr ++ ["ДПМная часть - ДПМ не до конца года - Дата прекращения ДПМ: " <> Text.pack (show finish)]
                  }
              ]
            else [gr]