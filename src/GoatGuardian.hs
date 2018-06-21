{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module GoatGuardian where

import Control.Monad.Catch (try)
import Control.Monad.IO.Class
import Control.Monad.Reader (ask)
import Data.ByteString (ByteString)
import Data.Semigroup ((<>))
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Database.Persist ((==.), deleteWhere, insert_)
import Database.Persist.TH (mkMigrate, mkPersist, mpsGenerateLenses, persistLowerCase, share, sqlSettings)
import Network.HTTP.Conduit (HttpException, Manager, parseRequest, newManager, tlsManagerSettings)
import qualified Network.HTTP.Conduit as HTTPClient
import Network.HTTP.ReverseProxy
import Network.HTTP.Types.Header (hLocation)
import Network.HTTP.Types.Status (status302)
import Network.Wai (Request, Response, ResponseReceived, pathInfo, requestHeaders, responseLBS)
import Network.Wai.Handler.Warp
import System.Envy
import Tonatona (Plug(..), TonaM, readerConf, readerShared)
import qualified Tonatona as Tona
import Tonatona.Db.Sqlite (TonaDbConfig, TonaDbSqlShared)
import qualified Tonatona.Db.Sqlite as TonaDb
import Tonatona.Logger (TonaLoggerShared(..), logDebug, logInfo, stdoutLogger)
import qualified Tonatona.Logger as TonaLogger
import Web.Authenticate.OAuth (Credential(..), authorizeUrl, getTemporaryCredential)
import Web.Twitter.Conduit (OAuth(..), twitterOAuth)

$(share
  [ mkPersist sqlSettings {mpsGenerateLenses = False}
  , mkMigrate "migrateAll"
  ]
  [persistLowerCase|
  TwitterTemporaryToken
    token       Text
    secret      Text

    deriving Eq
    deriving Show
    |]
 )

data TwitterConfig = TwitterConfig
  { twitterOAuthCallbackUrl :: ByteString
  , twitterOAuthKey :: ByteString
  , twitterOAuthSecret :: ByteString
  } deriving Show

instance FromEnv TwitterConfig where
  fromEnv = do
    TwitterConfig
      <$> envMaybe "GG_TWITTER_OAUTH_CALLBACK_URL" .!= "http://localhost:3000/twitter/callback"
      <*> env "GG_TWITTER_OAUTH_KEY"
      <*> env "GG_TWITTER_OAUTH_SECRET"

data Config = Config
  { tonaDb :: TonaDb.Config
  , twitterConfig :: TwitterConfig
  }
  deriving (Show)

instance FromEnv Config where
  fromEnv =
    Config
      <$> fromEnv
      <*> fromEnv

instance TonaDbConfig Config where
  config = tonaDb

data Shared = Shared
  { httpManager :: Manager
  , tonaDb :: TonaDb.Shared
  , tonaLogger :: TonaLogger.Shared
  }

instance Plug Config Shared where
  init conf = do
    Shared
      <$> newManager tlsManagerSettings
      <*> TonaDb.init conf stdoutLogger
      <*> TonaLogger.init stdoutLogger

instance TonaDbSqlShared Shared where
  shared = tonaDb

instance TonaLoggerShared Shared where
  shared = tonaLogger

type Tona = TonaM Config Shared

defaultMain :: IO ()
defaultMain =
  Tona.run $ do
    TonaDb.runMigrate migrateAll
    (conf, shared) <- ask
    liftIO $ run 3000 $ app conf shared

handleTwitterLogin :: Request -> Tona WaiProxyResponse
handleTwitterLogin req = do
  $(logDebug) $ "handleTwitterLogin, started..."
  twitConf <- readerConf twitterConfig
  let tokens =
        twitterOAuth
          { oauthConsumerKey = twitterOAuthKey twitConf
          , oauthConsumerSecret = twitterOAuthSecret twitConf
          , oauthCallback = Just $ twitterOAuthCallbackUrl twitConf
          }
  manager <- readerShared httpManager
  $(logDebug) $ "handleTwitterLogin, about to run getTemporaryCredential..."
  eitherCred <- try $ getTemporaryCredential tokens manager
  $(logDebug) $ "handleTwitterLogin, eitherCred: " <> tshow eitherCred
  case eitherCred of
    Left (err :: HttpException) -> do
      undefined
    Right (Credential creds) -> do
      let maybeCredRes = do
            confirmed <- lookup "oauth_callback_confirmed" creds
            token <- lookup "oauth_token" creds
            secret <- lookup "oauth_token_secret" creds
            pure (confirmed, token, secret)
      case maybeCredRes of
        Nothing -> do
          $(logDebug) "handleTwitterLogin, couldn't fnid values"
          undefined
        Just ("true", token, secret) -> do
          $(logDebug) "handleTwitterLogin, successfully looked up values"
          -- TODO: It might be possible just to encrypt these values
          -- and return them to the user in a cookie that we can read
          -- later.
          TonaDb.run $ do
            let textToken = decodeUtf8With lenientDecode token
                textSecret = decodeUtf8With lenientDecode secret
            deleteWhere [TwitterTemporaryTokenToken ==. textToken]
            insert_ (TwitterTemporaryToken textToken textSecret)
          let url = authorizeUrl tokens (Credential creds)
          let bytestringUrl = encodeUtf8 $ pack url
          $(logDebug) $ "handleTwitterLogin, url: " <> tshow url
          let resp = responseLBS status302 [(hLocation, bytestringUrl)] mempty
          pure $ WPRResponse resp
        _ -> do
          $(logDebug) "handleTwitterLogin, response from twitter didn't have oauth_callback_confirmed"
          undefined

     -- cred <- OA.getTemporaryCredential tokens mgr
     -- case lookup "oauth_token" $ unCredential cred of
     --     Just temporaryToken -> do
     --         liftIO $ storeCredential temporaryToken cred usersToken
     --         let url = OA.authorizeUrl tokens cred
     --         redirect $ LT.pack url
     --     Nothing -> do
     --         status HT.status500
     --         text "Failed to obtain the temporary token."

handleProxy :: Request -> Tona WaiProxyResponse
handleProxy req = do
  -- TODO: Check if the user is authenticated, and if so then add the X-UserId
  -- header to the request.
  let oldReqHeaders = requestHeaders req
  let newReq = req { requestHeaders = ("X-UserId", "1") : oldReqHeaders }
  pure $ WPRModifiedRequest newReq (ProxyDest "localhost" 8000)

router :: Request -> Tona WaiProxyResponse
router req = do
  -- TODO: Make sure the X-UserId header is removed from the incoming request.
  let reqPath = pathInfo req
  case reqPath of
    "twitter":"login":_ -> handleTwitterLogin req
    _ -> handleProxy req

app :: Config -> Shared -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app conf shared req respF = do
  let waiProxySettings =
        defaultWaiProxySettings
          { wpsSetIpHeader = SIHFromHeader
          , wpsProcessBody = \_ _ -> Nothing
          }
  waiProxyToSettings
    (Tona.runWithConfAndShared conf shared . router)
    waiProxySettings
    (httpManager shared)
    req
    respF

tshow :: Show a => a -> Text
tshow = pack . show
