{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lib where

import Prelude hiding (head)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Except (throwError)
import Data.Foldable (forM_)
import Data.Proxy (Proxy(Proxy))
import Data.Semigroup ((<>))
import Data.Text (Text, unpack)
import Data.Void (Void, absurd)
import Database.Persist.Sqlite (Entity(Entity), SqlBackend, (==.), fromSqlKey, insert_, runMigration, runSqlConn, selectList, withSqliteConn)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Network.HTTP.Types.Header (hLocation)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant (Application, Get, FormUrlEncoded, Handler, Header', Optional, Post, ReqBody, Required, Server, Strict, (:>), (:<|>)((:<|>)), err302, errHeaders, serve)
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html5 (Html, ToMarkup(toMarkup), (!), a, body, form, h1, h3, head, hr, html, input, li, p, title, toHtml, ul)
import Text.Blaze.Html5.Attributes (action, href, method, name, type_, value)
import Web.FormUrlEncoded (Form, FromForm(fromForm), lookupUnique)

import Types (UserId(unUserId))

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
type OptionalUserIdHeader = Header' '[Optional, Strict] "X-UserId" UserId

newtype PostContents = PostContents { unPostContents :: Text } deriving (Eq, Show)

instance FromForm PostContents where
  fromForm :: Form -> Either Text PostContents
  fromForm frm = PostContents <$> lookupUnique "contents" frm

instance ToMarkup Void where
  toMarkup void = absurd void

type API =
  OptionalUserIdHeader :> Get '[HTML] Html :<|>
  "after-login" :> UserIdHeader :> Get '[HTML] Html :<|>
  "after-login" :> UserIdHeader :> ReqBody '[FormUrlEncoded] PostContents :> Post '[HTML] Void :<|>
  "all-posts" :> OptionalUserIdHeader :> Get '[HTML] Html :<|>
  "email-login-page" :> Get '[HTML] Html :<|>
  "email-register-page" :> Get '[HTML] Html :<|>
  "email-change-pass-page" :> Get '[HTML] Html :<|>
  "email-reset-pass-send-email-page" :> Get '[HTML] Html :<|>
  "email-reset-pass-page" :> Get '[HTML] Html

server :: SqlBackend -> Server API
server sqlBackend =
  getHomePage :<|>
  getAfterLogin sqlBackend :<|>
  postAfterLogin sqlBackend :<|>
  getAllPosts sqlBackend :<|>
  getEmailLoginPage :<|>
  getEmailRegisterPage :<|>
  getEmailChangePassPage :<|>
  getEmailResetPassSendEmailPage :<|>
  getEmailResetPassPage

getHomePage :: Maybe UserId -> Handler Html
getHomePage maybeUserId = do
  -- liftIO $ "starting getHomePage"
  pure $
    html $ do
      head $ title "Example Servant App"
      body $ do
        h1 $ "Example Servant App Homepage"
        h3 $
          "Logged In User? " <>
            maybe "(not logged in)" (toMarkup . unUserId) maybeUserId
        h3 $ "Log In?"
        p $
          a ! href "http://localhost:3000/twitter/login" $ "login with twitter"
        hr
        p $
          a ! href "http://localhost:3000/email-login-page" $ "login with email"
        p $
          a ! href "http://localhost:3000/email-register-page" $ "register with email"
        p $
          a ! href "http://localhost:3000/email-change-pass-page" $
            "change password for email login"
        p $
          a ! href "http://localhost:3000/email-reset-pass-send-email-page" $
            "reset your password if you have forgotten it"
        h3 $ "All Posts"
        p $
          a ! href "http://localhost:3000/all-posts" $ "See all posts"

getAfterLogin :: SqlBackend -> UserId -> Handler Html
getAfterLogin sqlBackend userId = do
  blogPostEntities <-
    runDb sqlBackend $ selectList [ BlogPostAuthor ==. userId ] []
  pure $
    html $ do
      head $ title "Example Servant App"
      body $ do
        h1 $ "Example Servant App for logged-in User"
        h3 $ "Logged In User"
        p $ toHtml $ "logged in as user: " <> show (unUserId userId)
        p $
          a ! href "http://localhost:3000/logout?next=http%3A%2F%2Flocalhost%3A3000" $ "logout"
        h3 $ "Create Blog Post"
        form ! method "POST" $ do
          p $ do
            "contents"
            (input ! type_ "text" ! name "contents")
          input ! type_ "submit" ! value "Submit"
        h3 $ "Blog Posts"
        ul $
          forM_ blogPostEntities $ \(Entity blogPostId blogPost) ->
            li . toHtml $
              "id " <> show (fromSqlKey blogPostId) <> ": " <> unpack (blogPostContent blogPost)

postAfterLogin :: SqlBackend -> UserId -> PostContents -> Handler Void
postAfterLogin sqlBackend userId (PostContents contents) = do
  runDb sqlBackend $ insert_ (BlogPost userId contents)
  throwError $ err302 { errHeaders = [(hLocation, "http://localhost:3000/after-login")] }

getAllPosts :: SqlBackend -> Maybe UserId -> Handler Html
getAllPosts sqlBackend maybeUserId = do
  blogPostEntities <- runDb sqlBackend $ selectList [] []
  pure $
    html $ do
      head $ title "Example Servant App"
      body $ do
        h1 $ "All blog posts"
        h3 $ "Logged In User"
        p $
          toHtml $
            case maybeUserId of
              Just userId -> "logged in as user: " <> show (unUserId userId)
              Nothing -> "(not logged in)"
        h3 $ "Blog Posts"
        ul $
          forM_ blogPostEntities $ \(Entity blogPostId blogPost) ->
            li . toHtml $
              "id " <> show (fromSqlKey blogPostId) <> ": " <> unpack (blogPostContent blogPost)

getEmailLoginPage :: Handler Html
getEmailLoginPage = do
  pure $
    html $ do
      head $ title "Example Servant App"
      body $ do
        h1 $ "Email Login"
        form ! method "POST" ! action "/email/login" $ do
          p $ do
            "email"
            (input ! type_ "text" ! name "email")
          p $ do
            "password"
            (input ! type_ "text" ! name "password")
          input ! type_ "submit" ! value "Submit"

getEmailRegisterPage :: Handler Html
getEmailRegisterPage = do
  pure $
    html $ do
      head $ title "Example Servant App"
      body $ do
        h1 $ "Email Register"
        form ! method "POST" ! action "/email/register" $ do
          p $ do
            "email"
            (input ! type_ "text" ! name "email")
          p $ do
            "password"
            (input ! type_ "text" ! name "password")
          input ! type_ "submit" ! value "Submit"

getEmailChangePassPage :: Handler Html
getEmailChangePassPage = do
  pure $
    html $ do
      head $ title "Example Servant App"
      body $ do
        h1 $ "Change Email Login Password"
        form ! method "POST" ! action "/email/change-password" $ do
          p $ do
            "old password"
            (input ! type_ "text" ! name "old-pass")
          p $ do
            "new password"
            (input ! type_ "text" ! name "new-pass")
          input ! type_ "submit" ! value "Submit"

getEmailResetPassSendEmailPage :: Handler Html
getEmailResetPassSendEmailPage = do
  pure $
    html $ do
      head $ title "Example Servant App"
      body $ do
        h1 $ "Send Email to Reset Email Login Password"
        form ! method "POST" ! action "/email/reset-password-send-email" $ do
          p $ do
            "email"
            (input ! type_ "text" ! name "email")
          input !
            type_ "hidden" !
            name "next" !
            value "http://localhost:3000/email-reset-pass-page"
          input ! type_ "submit" ! value "Submit"
          p "You will be sent an email with a link allowing you to change you password."

getEmailResetPassPage :: Handler Html
getEmailResetPassPage = do
  pure $
    html $ do
      head $ title "Example Servant App"
      body $ do
        h1 $ "Reset Email Login Password"
        form ! method "POST" ! action "/email/reset-password" $ do
          p $ do
            "new password"
            (input ! type_ "text" ! name "new-pass")
          input ! type_ "hidden" ! name "next" ! value "http://localhost:3000/"
          input ! type_ "submit" ! value "Submit"

runDb :: MonadIO m => SqlBackend -> ReaderT SqlBackend IO a -> m a
runDb sqlBackend query = liftIO $ runSqlConn query sqlBackend

app :: SqlBackend -> Application
app sqlBackend = serve (Proxy @API) (server sqlBackend)

defaultMain :: IO ()
defaultMain =
  runNoLoggingT $
    withSqliteConn "example-servant-app.sqlite3" $ \sqlBackend -> liftIO $ do
      runDb sqlBackend $ runMigration migrateAll
      let port = 8000
      putStrLn $ "Running example-servant-app on port " <> show port <> "..."
      run port . logStdoutDev $ app sqlBackend
