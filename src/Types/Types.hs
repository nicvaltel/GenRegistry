module Types.Types where

import Data.Text (Text)
import Data.Time.Calendar (Day)

class ShowText a where
  showText :: a -> Text

type ErrorMsg = String

newtype Warning = Warning {unWarning :: String}
  deriving (Show, Eq, Ord)

type StationCode = String

type GTPGCode = String

type Pust = Float

data StationCategory = BlockStation | NormalStation
  deriving (Show, Eq)

data StationType = AES | VES | GRES_KES_TEC | GES | SES | TES
  deriving (Show, Eq)

data PriceZone = EUR | SIB
  deriving (Show, Eq)

data XAttrType = EMPTY_XATTR_TYPE | IS_RENEWABLE_DPM | IS_MOBILE
  deriving (Show, Eq)

data GemSelectionResult
  = GSRSelected
  | GSRNotSelected
  | GSRNotSelectedSupplyAfterMod
  | GSRNotSelectedRefusalToFillRequest
  | GSRNotSelectedMinimumTechnicalRequirements
  | GSRNotSelectedNoTechnicalSupport
  | GSRNotSelectedZeroInDecember
  | GSRNotSelectedWithZeroYearlyVolume
  | GSRMVR
  | GSRDPM
  deriving (Show, Eq)

data MVRType = MVR_EE | MVR_HEAT
  deriving (Show)

data ConstantsAndDates = ConstantsAndDates
  { cndYear :: Int,
    cndVrBanDate :: Day,
    cndStartYearDate :: Day,
    cndFinishYearDate :: Day,
    cndFirstApril :: Day,
    cndSecondTermDate :: Day,
    cndMinPust :: Pust,
    cndYear2007 :: Int,
    cndYear2011 :: Int
  }
  deriving (Show)

type GaCode = Int

type GaCode2007_2011 = GaCode

data VRParams = VRParams {vrStartDate :: Day, vrFinishDate :: Day, vrIsVrZapret :: Bool, vr2007_2011 :: Bool}

data DPMParams = DPMParams {dpmStartDate :: Day, dpmFinishDate :: Day}

data KommodParams = KommodParams {kommodStartDate :: Day, kommodFinishDate :: Day, kommodSupplyStartDate :: Day}

data NGOParams = NGOParams {ngoStartDate :: Day, ngoFinishDate :: Day}

newtype RPRF2699Params = RPRF2699Params {rprf2699StartDate :: Day}

newtype VyvodSoglasovan = VyvodSoglasovan {vyvodSoglasovanDate :: Day}

data SupplyAttribute = NoSupply | SupplyAllYear | SupplyPeriod {supplyPeriodFrom :: Day, supplyPeriodTo :: Day}
  deriving (Show, Eq)
