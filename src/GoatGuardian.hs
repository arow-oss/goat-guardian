module GoatGuardian where

import Network.HTTP.Conduit (newManager, tlsManagerSettings)
import Network.HTTP.ReverseProxy
import Network.Wai
import Network.Wai.Handler.Warp

defaultMain :: IO ()
defaultMain = run 3000 app

router :: Request -> IO WaiProxyResponse
router req = do
  -- TODO: Make sure the X-UserId header is pulled out of the incoming request.
  pure $ WPRModifiedRequest req (ProxyDest "localhost" 8000)

app :: Application
app req respF = do
  manager <- newManager tlsManagerSettings
  let waiProxySettings =
        defaultWaiProxySettings
          { wpsSetIpHeader = SIHFromHeader
          -- TODO: Change this so that it removes the X-UserId header.
          , wpsProcessBody = \_ _ -> Nothing
          }
  waiProxyToSettings router waiProxySettings manager req respF
