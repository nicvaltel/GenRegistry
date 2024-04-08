{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Types where

import Data.Csv (FromNamedRecord (..), (.:))
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Calendar (Day)

type StationCode = String
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
  | GSRMVR
  | GSRDPM
  deriving (Show)

data MVRType = MVR_EE | MVR_HEAT
  deriving (Show)



data ConstantsAndDates = ConstantsAndDates
  { cndYear :: Int,
    cndVrBanDate :: Day,
    cndStartYearDate :: Day,
    cndFinishYearDate :: Day,
    cndFirstApril :: Day,
    cndSecondTermDate :: Day,
    cndMinPust :: Pust
  }
  deriving (Show)
