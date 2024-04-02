{-# LANGUAGE OverloadedStrings #-}

module Utils where
import Data.Text (Text)
import qualified Data.Text as Text



dateConvert :: String -> String -> Text -> Text
dateConvert _ _ "" = ""
dateConvert "mm/dd/yyyy" "yyyy-mm-dd" str = 
    case Text.split (== '/') str of
        [mm,dd,yyyy] -> 
            let mm' = if Text.length mm == 1 then "0" <> mm else mm
                dd' = if Text.length dd == 1 then "0" <> dd else dd
            in yyyy <> "-" <> mm' <> "-" <> dd'
        _ -> error $ "dateConvert: wrong input date" <> show str
dateConvert _ _ _  = error "dateConvert: wrong date convert format "