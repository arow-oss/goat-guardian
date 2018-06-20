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
defaultMain = run 3000 app

handleTwitterLogin :: Request -> Tona WaiProxyResponse
handleTwitterLogin = undefined

handleProxy :: Request -> Tona WaiProxyResponse
handleProxy req =
  pure $ WPRModifiedRequest req (ProxyDest "localhost" 8000)

router :: Request -> Tona WaiProxyResponse
router req = do
  -- TODO: Make sure the X-UserId header is removed from the incoming request.
  let reqPath = pathInfo req
  case reqPath of
    "twitter":"login":_ -> handleTwitterLogin req
    _ -> handleProxy req

app :: Application
app req respF = do
  let waiProxySettings =
        defaultWaiProxySettings
          { wpsSetIpHeader = SIHFromHeader
          , wpsProcessBody = \_ _ -> Nothing
          }
  Tona.run $ do
    (conf, shared) <- ask
    manager <- readerShared httpManager
    liftIO $
      waiProxyToSettings
        (Tona.runWithConfAndShared conf shared . router)
        waiProxySettings
        manager
        req
        respF
