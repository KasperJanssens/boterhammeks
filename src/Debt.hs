{-# LANGUAGE OverloadedStrings #-}

module Debt where

import Database.PostgreSQL.Simple.FromRow
import qualified Snap.Snaplet.PostgresqlSimple as SnapPostgres
import           Snap.Core
import           Snap.Snaplet
import Data.Text
import Control.Applicative
import Application
import Control.Exception.Base
import Control.Monad

data Debt = Debt {
  name :: Text,
  debt :: Double
} deriving Show

instance FromRow Debt where
  fromRow = Debt <$> field <*> field

addDebt :: Text -> Text -> Handler App App ()
addDebt name debt = do
  newDebt <- SnapPostgres.execute "insert into debts values (?,?)" (name, debt)

  redirect "/"

getAllDebts :: Handler App App [Debt]
getAllDebts = SnapPostgres.query_ "select * from debts"


merge :: Debt -> Debt -> Debt
merge debt1 debt2 =
  assert
    (name debt1 == name debt2)
  Debt (name debt1) (debt debt1 + debt debt2)

insert :: Debt -> Handler App App ()
insert (Debt name debt) =
  void $ SnapPostgres.execute "insert into debts values (?,?)" (name, debt)


update :: Debt -> Handler App App ()
update  (Debt name debt) =
  void $ SnapPostgres.execute "update debts set debt = ? where name = ?" (debt, name)
