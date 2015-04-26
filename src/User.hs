module User where

import Data.Functor

import qualified Data.Text as Text
import Database.PostgreSQL.Simple.FromRow

data User = User {
  name :: Text.Text
}

instance FromRow User where
  fromRow= User <$> field
