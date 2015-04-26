{-# LANGUAGE  OverloadedStrings #-}
module Boodschap where

import           Text.Digestive
import           Text.Digestive.Heist
import           Text.Digestive.Snap
import qualified Data.Text as Text
import qualified Data.Text.Read as Read
import qualified Data.List as List
import User
import Control.Applicative

data MeeEter = MeeEter {
  eetMee :: Bool,
  naam :: Text.Text
} deriving (Show, Eq)

meeEetForm :: (Monad m) => Maybe MeeEter -> Form Text.Text m MeeEter
meeEetForm maybeMeeter  = MeeEter <$> "eetMee" .: bool (fmap eetMee maybeMeeter)
                                  <*> "naam" .: text (fmap naam maybeMeeter)

data Boodschap = Boodschap {
  boodschapper :: Text.Text,
  bedrag :: Double,
  veleEters :: [MeeEter]
} deriving (Show, Eq)

bedragErrMsg :: Text.Text
bedragErrMsg = "Misschien toch 't bedrag invullen?"

boodschapForm :: (Monad m ) => [User] -> Form Text.Text m Boodschap
boodschapForm users =
 let names = name <$> users in
 Boodschap
 <$> "boodschapper" .: choice (zip names names) Nothing
 <*> "bedrag" .: stringRead "geen correct getal" (Just 0.0)
 <*> "names" .: (filter eetMee <$> listOf meeEetForm (Just $ MeeEter <$> [False] <*> names))