{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module GoatGuardian where

import Control.Monad.Catch (try)
import Control.Monad.IO.Class
import Control.Monad.Reader (ask)
import Data.ByteString (ByteString)
import Data.Semigroup ((<>))
import Data.Text (Text, pack)
import Network.HTTP.Conduit (HttpException, Manager, parseRequest, newManager, tlsManagerSettings)
import qualified Network.HTTP.Conduit as HTTPClient
import Network.HTTP.ReverseProxy
import Network.Wai (Request, Response, ResponseReceived, pathInfo, requestHeaders)
import Network.Wai.Handler.Warp
import System.Envy
import Tonatona (Plug(..), TonaM, readerConf, readerShared)
import qualified Tonatona as Tona
import Tonatona.Logger (TonaLoggerShared(..), logDebug, logInfo, stdoutLogger)
import qualified Tonatona.Logger as TonaLogger
import Web.Authenticate.OAuth (getTemporaryCredential)
import Web.Twitter.Conduit (OAuth(..), twitterOAuth)

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
  { twitterConfig :: TwitterConfig
  }
  deriving (Show)

instance FromEnv Config where
  fromEnv = Config <$> fromEnv

data Shared = Shared
  { tonaLogger :: TonaLogger.Shared
  , httpManager :: Manager
  }

instance Plug Config Shared where
  init conf = do
    Shared
      <$> TonaLogger.init stdoutLogger
      <*> newManager tlsManagerSettings

instance TonaLoggerShared Shared where
  shared = tonaLogger

type Tona = TonaM Config Shared

defaultMain :: IO ()
defaultMain =
  Tona.run $ do
    (conf, shared) <- ask
    liftIO $ run 3000 $ app conf shared

handleTwitterLogin :: Request -> Tona WaiProxyResponse
handleTwitterLogin req = do
  -- oauthUrlReq <- readerShared twitterOAuthUrlReq
  -- TwitterConfig{twitterOAuthKey, twitterOAuthSecret} <- readerConf twitterConfig
  -- let cred = clientCred $ Token twitterOAuthKey twitterOAuthSecret
  -- oauthReq <- oauth cred defaultServer oauthUrlReq
  -- putStrLn $ "oauth req: " <> show oauthReq
  -- manager <- readerShared httpManager
  -- res <- httpLbs oauthReq manager
  -- putStrLn $ "oauth resp: " <> show res
  -- undefined
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
            secret <- lookup "oauth_secret" creds
            pure (confirmed, token, secret)
      case maybeCredRes of
        Nothing -> undefined
        Just ("true", token, secret) -> do
        _ -> do
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
