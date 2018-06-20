module GoatGuardian where

import Network.HTTP.Conduit (newManager, tlsManagerSettings)
import Network.HTTP.ReverseProxy
import Network.Wai
import Network.Wai.Handler.Warp

defaultMain :: IO ()
defaultMain = run 3000 app

handleTwitterLogin :: Request -> IO WaiProxyResponse
handleTwitterLogin = undefined

handleProxy :: Request -> IO WaiProxyResponse
handleProxy req =
  pure $ WPRModifiedRequest req (ProxyDest "localhost" 8000)

router :: Request -> IO WaiProxyResponse
router req = do
  -- TODO: Make sure the X-UserId header is removed from the incoming request.
  let reqPath = pathInfo req
  case reqPath of
    "twitter":"login":_ -> handleTwitterLogin req
    _ -> handleProxy req

app :: Application
app req respF = do
  manager <- newManager tlsManagerSettings
  let waiProxySettings =
        defaultWaiProxySettings
          { wpsSetIpHeader = SIHFromHeader
          , wpsProcessBody = \_ _ -> Nothing
          }
  waiProxyToSettings router waiProxySettings manager req respF
