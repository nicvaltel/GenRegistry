module ShowText where


import Data.Text (Text)



class ShowText a where
    showText :: a -> Text