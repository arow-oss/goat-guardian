{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Prelude hiding (head)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Database.Persist.Sqlite (SqlBackend, withSqliteConn)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant (Application, Get, Handler, Header', Required, Server, Strict, (:>), (:<|>)((:<|>)), serve)
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html5 (Html, (!), a, body, head, html, p, title)
import Text.Blaze.Html5.Attributes (href)

import Types (UserId(UserId))

$(share
  [mkPersist sqlSettings , mkMigrate "migrateAll"]
  [persistLowerCase|
  BlogPost
    author       UserId
    content      Text

    deriving Eq
    deriving Show
    |]
 )

type UserIdHeader = Header' '[Required, Strict] "X-UserId" UserId

type API =
  Get '[HTML] Html :<|>
  "after-login" :> UserIdHeader :> Get '[HTML] Html -- :<|>

server :: SqlBackend -> Server API
server sqlBackend = homePage :<|> afterLogin sqlBackend

homePage :: Handler Html
homePage = do
  pure $
    html $ do
      head $ title "Example Servant App"
      body $
        p $
          a ! href "http://localhost:3000/twitter/login" $ "login with twitter"

afterLogin :: SqlBackend -> UserId -> Handler Html
afterLogin sqlBackend userId = undefined

app :: SqlBackend -> Application
app sqlBackend = serve (Proxy @API) (server sqlBackend)

defaultMain :: IO ()
defaultMain =
  runNoLoggingT $
    withSqliteConn "example-servant-app.sqlite3" $ \sqlBackend ->
      liftIO . run 8000 . logStdoutDev $ app sqlBackend
