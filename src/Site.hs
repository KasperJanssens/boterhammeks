{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import qualified Snap.Snaplet.PostgresqlSimple as SnapPostgres
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
import Control.Monad.State.Class
import Snap.Snaplet.PostgresqlSimple
import qualified Debt
import qualified Boodschap
import qualified Text.Digestive.Snap as DigestiveSnap
import qualified Text.Digestive.Heist as DigestiveHeist
import           Data.List
import qualified Data.Text as Text
import qualified Data.Double.Conversion.Text as DoubleConversion
------------------------------------------------------------------------------
import           Application
import User
import Data.Function


------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe noSplices splice authError
    splice err = "loginError" ## I.textSplice err


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/overview")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"

getAllUsers :: Handler App App [User]
getAllUsers = SnapPostgres.query_ "select login from snap_auth_user"

handleOverview :: [Debt.Debt] -> Handler App (AuthManager App) ()
handleOverview allDebts =
  heistLocal (I.bindSplices ("schuld" ## spliceDebts allDebts)) $ render "lijst"
  where spliceDebts = I.mapSplices (I.runChildrenWith . debtSplices)
        debtSplices (Debt.Debt name debt) = do
          "name" ## I.textSplice name
          "debt" ## I.textSplice $ DoubleConversion.toShortest debt

boodschapFormHandler :: Handler App App ()
boodschapFormHandler = do
  allUsers <- getAllUsers
  (view, result) <- DigestiveSnap.runForm "boodschap" $ Boodschap.boodschapForm allUsers
  case result of
    Just boodschap -> do
      let newDebts  = calculateNewDebts boodschap
      currentDebts <- Debt.getAllDebts
      let dbaseInstructions = foldl (
                \ dbInstructions newDebtForX -> case findIndex (((==) `on` Debt.name) newDebtForX) currentDebts of
                                            Nothing -> Debt.insert newDebtForX : dbInstructions
                                            Just index ->
                                                let oldDebtForX = currentDebts !! index in
                                                Debt.update (Debt.merge oldDebtForX newDebtForX) : dbInstructions
            ) [] newDebts
      sequence_ dbaseInstructions
      redirect "/"
    Nothing -> heistLocal (DigestiveHeist.bindDigestiveSplices view) $ render "boodschap"


calculateNewDebts ::Boodschap.Boodschap -> [Debt.Debt]
calculateNewDebts (Boodschap.Boodschap boodschapper bedrag eters) =
  let bedragPerEter = (bedrag / fromIntegral (1 + length eters)) in
  let boodschapperDebt = Debt.Debt boodschapper (fromIntegral (length eters)  * bedragPerEter) in
  let newDebts = (\meeEter -> Debt.Debt (Boodschap.naam meeEter) (negate bedragPerEter)) <$> eters in
  boodschapperDebt : newDebts
------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",    with auth handleLoginSubmit)
         , ("/logout",   with auth handleLogout)
         , ("/new_user", with auth handleNewUser)
         , ("/boodschap",boodschapFormHandler)
         , ("/overview", do
              allDebts <- Debt.getAllDebts
              with auth $ handleOverview allDebts)
         , ("",          serveDirectory "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "A snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    d <- nestSnaplet "db" db pgsInit

    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $ initPostgresAuth sess d
    addRoutes routes
    addAuthSplices h auth
    return $ App h s d a

