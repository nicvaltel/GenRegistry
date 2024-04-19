{-# LANGUAGE OverloadedStrings #-}

module Utils (dateConvert, putStrUtf8, putStrLnUtf8, putTextUtf8, putTextLnUtf8, countElems) where

import qualified Data.ByteString.Char8 as C8 (putStr, putStrLn)
import Data.ByteString.UTF8 (fromString)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Prelude hiding (putStr, putStrLn)

dateConvert :: String -> String -> Text -> Text
dateConvert _ _ "" = ""
dateConvert "mm/dd/yyyy" "yyyy-mm-dd" str =
  case Text.split (== '/') str of
    [mm, dd, yyyy] ->
      let mm' = if Text.length mm == 1 then "0" <> mm else mm
          dd' = if Text.length dd == 1 then "0" <> dd else dd
       in yyyy <> "-" <> mm' <> "-" <> dd'
    _ -> error $ "dateConvert: wrong input date" <> show str
dateConvert _ _ _ = error "dateConvert: wrong date convert format "

putStrUtf8 :: String -> IO ()
putStrUtf8 = C8.putStr . fromString

putStrLnUtf8 :: String -> IO ()
putStrLnUtf8 = C8.putStrLn . fromString

putTextUtf8 :: Text -> IO ()
putTextUtf8 = C8.putStr . encodeUtf8

putTextLnUtf8 :: Text -> IO ()
putTextLnUtf8 = C8.putStrLn . encodeUtf8

countElems :: (Ord a) => [a] -> [(a, Int)]
countElems xs =
  Map.toList $
    foldr (\x m -> case Map.lookup x m of Just n -> Map.insert x (n + 1) m; Nothing -> Map.insert x 1 m) Map.empty xs