{-# LANGUAGE  OverloadedStrings #-}
module Boodschap where

import           Text.Digestive
import           Text.Digestive.Heist
import           Text.Digestive.Snap
import qualified Data.Text as Text
import qualified Data.List as List
import Control.Applicative

data Boodschap = Boodschap {
  boodschapper :: Text.Text,
  bedrag :: Text.Text,
  meeEter :: Text.Text,
  veleEters :: [Text.Text]
} deriving (Show, Eq)

boodschapperErrMsg :: Text.Text
boodschapperErrMsg = "De boodschappendoender mag niet ontbreken"

bedragErrMsg :: Text.Text
bedragErrMsg = "Misschien toch 't bedrag invullen?"

meeEtersErrMsg :: Text.Text
meeEtersErrMsg = "Ge hebt alleen gegeten of zo?"

boodschapForm :: (Monad m ) => Form Text.Text m Boodschap
boodschapForm = Boodschap
 <$> "boodschapper" .: check boodschapperErrMsg (not . Text.null) (text Nothing)
 <*> "bedrag" .: check bedragErrMsg (not . Text.null) (text Nothing)
 <*> "etendecollegae" .: check meeEtersErrMsg (not . Text.null) (text Nothing)
 <*> "meerderecollegae" .: listOf text Nothing