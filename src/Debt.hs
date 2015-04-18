{-# LANGUAGE OverloadedStrings #-}

module Debt where

import Database.PostgreSQL.Simple.FromRow
import qualified Snap.Snaplet.PostgresqlSimple as SnapPostgres
import           Snap.Core
import           Snap.Snaplet
import Data.Text
import Control.Applicative
import Application

data Debt = Debt {
  name :: Text,
  debt :: Text
} deriving Show

instance FromRow Debt where
  fromRow = Debt <$> field <*> field

addDebt :: Text -> Text -> Handler App App ()
addDebt name debt = do
  newDebt <- SnapPostgres.execute "insert into debts values (?,?)" (name, debt)
  redirect "/"

getAllDebts :: Handler App App [Debt]
getAllDebts = SnapPostgres.query_ "select * from debts"
