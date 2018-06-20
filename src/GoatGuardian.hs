module GoatGuardian where

import Control.Monad.IO.Class
import Control.Monad.Reader (ask)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Network.HTTP.Conduit (Manager, newManager, tlsManagerSettings)
import Network.HTTP.ReverseProxy
import Network.Wai
import Network.Wai.Handler.Warp
import System.Envy
import Tonatona (Plug(..), TonaM, readerShared)
import qualified Tonatona as Tona
import Tonatona.Logger (TonaLoggerShared(..), logDebug, logInfo, stdoutLogger)
import qualified Tonatona.Logger as TonaLogger

data TwitterConfig = TwitterConfig
  { twitterOAuthUrl :: Text
  , twitterOAuthKey :: ByteString
  , twitterOAuthSecret :: ByteString
  } deriving Show

instance FromEnv TwitterConfig where
  fromEnv =
    TwitterConfig
      <$> envMaybe "GG_TWITTER_OAUTH_URL" .!= "http://localhost:3000"
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
handleTwitterLogin = undefined

-- this example of using the oauthenticated library is from
-- https://github.com/tel/oauthenticated/blob/master/examples/oauth-authenticate.hs
-- main :: IO ()
-- main = do
--   Opts {..} <- parseArgs
--   req <- Client.parseRequest oauthUrl
--   rng <- Crypto.cprgCreate <$> Crypto.createEntropyPool
--   manager <- Client.newManager Client.defaultManagerSettings
--   let cred = OAuth.clientCred $ OAuth.Token oauthKey oauthSecret
--       app = App manager OAuth.defaultServer cred
--   _ :: Value <- fst <$> evalRWST (makeRequest =<< signRequest req) app rng
-- 	pure ()

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
