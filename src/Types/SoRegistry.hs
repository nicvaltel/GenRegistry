{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types.SoRegistry (SoRegistry (..)) where

import Data.Csv (FromNamedRecord (..), (.:))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding
import Data.Time.Calendar (MonthOfYear, Year)
import Text.Read (readMaybe)
import Types.Types (Pust, StationCode)

data SoRegistry = SoRegistry
  { sorTargetMonth :: (Year, MonthOfYear),
    sorTraderCode :: String,
    sorStationCode :: StationCode,
    sorGtpCode :: String,
    sorAttPower :: Float,
    sorUstPower :: Pust
  }
  deriving (Show)

instance FromNamedRecord SoRegistry where
  parseNamedRecord record = do
    sorTargetMonth <- getMonthOfYear <$> record .: encodeUtf8 "targetmonth"
    sorTraderCode <- record .: encodeUtf8 "trader-code"
    sorStationCode <- record .: encodeUtf8 "station-code"
    sorGtpCode <- record .: encodeUtf8 "gtp-code"
    sorAttPower <- record .: encodeUtf8 "att-power"
    sorUstPower <- record .: encodeUtf8 "ust-power"
    pure
      SoRegistry
        { sorTargetMonth,
          sorTraderCode,
          sorStationCode,
          sorGtpCode,
          sorAttPower,
          sorUstPower
        }
    where
      getMonthOfYear :: Text -> (Year, MonthOfYear)
      getMonthOfYear txt =
        case getMonthOfYear' of
          Just res -> res
          Nothing -> error $ "getMonthOfYear wrong text: " <> Text.unpack txt
        where
          getMonthOfYear' :: Maybe (Year, MonthOfYear)
          getMonthOfYear' = do
            let (yyyy, mm) = Text.splitAt 4 txt
            (yyyy', mm') <-
              if Text.length yyyy == 4 && Text.length mm == 2
                then Just (Text.unpack yyyy, Text.unpack mm)
                else Nothing
            year :: Year <- readMaybe yyyy'
            mon :: MonthOfYear <- readMaybe mm'
            pure (year, mon)
