{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types.SoRegistry where

import Data.Csv (FromNamedRecord (..), (.:))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding
import Data.Time.Calendar (MonthOfYear, Year)
import Text.Read (readMaybe)
import Types.Types (StationCode, Pust)

data SoRegistry = SoRegistry
  { sorTargetMonth :: (Year, MonthOfYear),
    sorTraderCode :: String,
    sorOfficialName :: String,
    sorStationCode :: StationCode,
    sorFullName :: String,
    sorGtpCode :: String,
    sorFullName2 :: String,
    sorAttPower :: Float,
    sorUstPower :: Pust,
    sorJAttTp :: Maybe Int,
    sorJPlace :: Maybe Int,
    sorJName :: String
  }
  deriving (Show)

instance FromNamedRecord SoRegistry where
  parseNamedRecord record = do
    sorTargetMonth <- getMonthOfYear <$> record .: encodeUtf8 "targetmonth"
    sorTraderCode <- record .: encodeUtf8 "trader-code"
    sorOfficialName <- record .: encodeUtf8 "official-name"
    sorStationCode <- record .: encodeUtf8 "station-code"
    sorFullName <- record .: encodeUtf8 "full-name"
    sorGtpCode <- record .: encodeUtf8 "gtp-code"
    sorFullName2 <- record .: encodeUtf8 "full-name2"
    sorAttPower <- record .: encodeUtf8 "att-power"
    sorUstPower <- record .: encodeUtf8 "ust-power"
    sorJAttTp <- record .: encodeUtf8 "j-att-tp"
    sorJPlace <- record .: encodeUtf8 "j-place"
    sorJName <- record .: encodeUtf8 "j-name"
    pure
      SoRegistry
        { sorTargetMonth,
          sorTraderCode,
          sorOfficialName,
          sorStationCode,
          sorFullName,
          sorGtpCode,
          sorFullName2,
          sorAttPower,
          sorUstPower,
          sorJAttTp,
          sorJPlace,
          sorJName
        }
    where
      getMonthOfYear :: Text -> (Year, MonthOfYear)
      getMonthOfYear txt =
        case getMonthOfYear' txt of
          Just res -> res
          Nothing -> error $ "getMonthOfYear wrong text: " <> Text.unpack txt
        where
          getMonthOfYear' :: Text -> Maybe (Year, MonthOfYear)
          getMonthOfYear' txt = do
            let (yyyy, mm) = Text.splitAt 4 txt
            (yyyy', mm') <-
              if Text.length yyyy == 4 && Text.length mm == 2
                then Just (Text.unpack yyyy, Text.unpack mm)
                else Nothing
            year :: Year <- readMaybe yyyy'
            mon :: MonthOfYear <- readMaybe mm'
            pure (year, mon)
