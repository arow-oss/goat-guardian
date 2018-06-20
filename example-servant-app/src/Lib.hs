{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Servant
import Network.Wai.Handler.Warp

type API =
  "foo" :> Get '[JSON] Int

server :: Server API
server = getFoo

getFoo :: Handler Int
getFoo = pure 1

app :: Application
app = serve (Proxy @API) server

defaultMain :: IO ()
defaultMain = run 8000 app
