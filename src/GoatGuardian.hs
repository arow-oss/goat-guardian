{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module GoatGuardian where

import Control.Monad (join, when)
import Control.Monad.Catch (try)
import Control.Monad.Except (ExceptT(ExceptT), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Control.FromSum (fromMaybeM)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.ByteString.Lazy.Builder (Builder, toLazyByteString)
import Data.Semigroup ((<>))
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8', decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy (fromStrict)
import qualified Data.Text.IO as Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.Persist.Sql (Entity(..), Key, (==.), (=.), deleteWhere, fromSqlKey, get, getBy, getEntity, insert, insert_, repsert, selectFirst, toSqlKey, update)
import Database.Persist.TH (mkMigrate, mkPersist, mpsGenerateLenses, persistLowerCase, share, sqlSettings)
import Network.HTTP.Conduit (HttpException, Manager, newManager, tlsManagerSettings)
import Network.HTTP.ReverseProxy
import Network.HTTP.Types.Header (hCookie, hLocation, hSetCookie)
import Network.HTTP.Types.Status (Status, status302, status400, status403, status404, status409, status500)
import Network.HTTP.Types.URI (urlEncode)
import Network.Wai (Request, Response, ResponseReceived, pathInfo, queryString, requestHeaders, responseLBS)
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Parse
import Text.Email.Validate (isValid)
import Text.Read (readMaybe)
import TonaParser (FromEnv(..), ParserRenames(ParserRenames, envVarRenames), (.||), argLong, defParserRenames, env, envDef, envVar, fromEnvWithRenames)
import Tonatona (Plug(..), TonaM, readerConf, readerShared)
import qualified Tonatona as Tona
import qualified Tonatona.Db.Sqlite as TonaDb
import Tonatona.Email.Sendmail (Address(Address), simpleMail')
import qualified Tonatona.Email.Sendmail as TonaEmail
import Tonatona.Logger (logDebug, stdoutLogger)
import qualified Tonatona.Logger as TonaLogger
import Web.Authenticate.OAuth (Credential(..), authorizeUrl, getAccessToken, getTemporaryCredential)
import Web.ClientSession (decrypt, encryptIO, randomKey)
import qualified Web.ClientSession as ClientSession
import Web.Cookie (SetCookie, defaultSetCookie, parseCookies, renderCookies, renderSetCookie, setCookieHttpOnly, setCookieMaxAge, setCookieName, setCookiePath, setCookieValue)
import Web.Twitter.Conduit (OAuth(..), twitterOAuth)

import GoatGuardian.CmdLineOpts (CmdLineOpts(..), RawSessionKey(..), initRawSessKeyOrFail, parseCmdLineOpts)
import GoatGuardian.Password (checkPass, hashPass)
import GoatGuardian.Types (LoginToken(..), createLoginToken)

$(share
  [ mkPersist sqlSettings {mpsGenerateLenses = False}
  , mkMigrate "migrateAll"
  ]
  [persistLowerCase|
  Email
    email             Text
    hashedPass        Text
    verified          Bool
    userId            UserId

    UniqueEmail email

    deriving Eq
    deriving Show

  EmailLoginToken
    emailId    EmailId
    loginToken LoginToken
    created    UTCTime

    deriving Eq
    deriving Show

  TwitterTemporaryToken
    token       Text
    secret      Text

    deriving Eq
    deriving Show

  TwitterUser
    token         Text
    secret        Text
    twitterUserId Text
    screenName    Text
    userId        UserId

    UniqueUserId twitterUserId

    deriving Eq
    deriving Show

  User
    created     UTCTime

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
    let callback =
          envDef
            ( envVar "GG_TWITTER_OAUTH_CALLBACK_URL" .||
              argLong "twitter-oauth-callback"
            )
            "http://localhost:3000/twitter/callback"
        key =
          env
            ( envVar "GG_TWITTER_OAUTH_KEY" .||
              argLong "twitter-oauth-key"
            )
        secret =
          env
            ( envVar "GG_TWITTER_OAUTH_SECRET" .||
              argLong "twitter-oauth-secret"
            )
    TwitterConfig <$> callback <*> key <*> secret

data Config = Config
  { tonaDb :: TonaDb.Config
  , twitterConfig :: TwitterConfig
  , rawSessionKey :: RawSessionKey
  , redirAfterLoginUrl :: Text
  }
  deriving (Show)

instance FromEnv Config where
  fromEnv =
    let redirAfterLogin =
          envDef
            ( envVar "GG_REDIR_AFTER_LOGIN_URL" .||
              argLong "redir-after-login-url"
            )
            "http://localhost:3000"
        db =
          fromEnvWithRenames
            defParserRenames
              { envVarRenames = [("DB_CONN_STRING", "GG_DB_CONN_STRING")]
              }
    in Config <$> db <*> fromEnv <*> fromEnv <*> redirAfterLogin

instance TonaDb.HasConfig Config where
  config = tonaDb

data Shared = Shared
  { httpManager :: Manager
  , tonaDb :: TonaDb.Shared
  , tonaLogger :: TonaLogger.Shared
  , sessionKey :: ClientSession.Key
  }

instance Plug Config Shared where
  init conf = do
    let rawSessKey = rawSessionKey conf
    Shared
      <$> newManager tlsManagerSettings
      <*> TonaDb.init conf stdoutLogger
      <*> TonaLogger.init stdoutLogger
      <*> initRawSessKeyOrFail rawSessKey

instance TonaDb.HasShared Shared where
  shared = tonaDb

instance TonaLogger.HasShared Shared where
  shared = tonaLogger

type Tona = TonaM Config Shared

data GenerateSessionKey = GenerateSessionKey

defaultMain :: IO ()
defaultMain = do
  CmdLineOpts{genSessKey} <- parseCmdLineOpts
  case genSessKey of
    Just _ -> do
      putStrLn "Generating a new session key that can be used with Goat Guardian."
      putStrLn "Please set this key in the GG_SESSION_KEY environment variable and"
      putStrLn "rerun Goat Guardian:\n"
      (key, _) <- randomKey
      Text.putStrLn $ decodeUtf8With lenientDecode $ Base64.encode key
    Nothing ->
      Tona.run $ do
        TonaDb.runMigrate migrateAll
        (conf, shared) <- ask
        let port = 3000 :: Int
        $(logDebug) $ "Running Goat Guardian on port " <> tshow port <> "..."
        liftIO . run port . logStdoutDev $ app conf shared

handleTwitterLogin :: Request -> Tona WaiProxyResponse
handleTwitterLogin _req = do
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
    Left (_err :: HttpException) -> do
      let resp =
            responseLBS
              status500
              []
              "<p>could not access Twitter to get temporary credentials</p>"
      pure $ WPRResponse resp
    Right (Credential creds) -> do
      let maybeCredRes = do
            confirmed <- lookup "oauth_callback_confirmed" creds
            token <- lookup "oauth_token" creds
            secret <- lookup "oauth_token_secret" creds
            pure (confirmed, token, secret)
      case maybeCredRes of
        Nothing -> do
          $(logDebug) "handleTwitterLogin, couldn't find values"
          let resp =
                responseLBS
                  status500
                  []
                  "<p>could not find Twitter oauth_* values in temporary credential call</p>"
          pure $ WPRResponse resp
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
          let resp =
                responseLBS
                  status500
                  []
                  "<p>response from twitter didn't have oauth_callback_confirmed set to true</p>"
          pure $ WPRResponse resp

handleTwitterCallback :: Request -> Tona WaiProxyResponse
handleTwitterCallback req = do
  $(logDebug) $ "handleTwitterCallback, req: " <> tshow req
  twitConf <- readerConf twitterConfig
  let oauth =
        twitterOAuth
          { oauthConsumerKey = twitterOAuthKey twitConf
          , oauthConsumerSecret = twitterOAuthSecret twitConf
          , oauthCallback = Just $ twitterOAuthCallbackUrl twitConf
          }
  manager <- readerShared httpManager
  let maybeParams = do
        reqToken <- join $ lookup "oauth_token" (queryString req)
        reqVerifier <- join $ lookup "oauth_verifier" (queryString req)
        pure (reqToken, reqVerifier)
  case maybeParams of
    Nothing -> do
      let deniedParam = join $ lookup "denied" (queryString req)
      case deniedParam of
        Nothing -> do
          let resp =
                responseLBS
                  status500
                  []
                  "<p>callback response from twitter didn't have oauth_token or oauth_verifier or denied</p>"
          pure $ WPRResponse resp
        Just _ -> do
          let resp =
                responseLBS
                  status403
                  []
                  "<p>call response from twitter was denied</p>"
          pure $ WPRResponse resp
    Just (reqToken, reqVerifier) -> do
      maybeTempToken <-
        TonaDb.run $
          selectFirst
            [TwitterTemporaryTokenToken ==. decodeUtf8With lenientDecode reqToken]
            []
      case maybeTempToken of
        Nothing -> do
          let resp =
                responseLBS
                  status404
                  []
                  "<p>in twitter callback, tempory token for user not found in database</p>"
          pure $ WPRResponse resp
        Just (Entity _ (TwitterTemporaryToken dbToken dbSecret)) -> do
          let cred =
                Credential
                  [ ("oauth_token", encodeUtf8 dbToken)
                  , ("oauth_token_secret", encodeUtf8 dbSecret)
                  , ("oauth_verifier", reqVerifier)
                  ]
          Credential accessTokens <- getAccessToken oauth cred manager
          $(logDebug) $ "handleTwitterCallback, accessTokens: " <> tshow accessTokens
          let maybeAccessTokens = do
                token <- lookup "oauth_token" accessTokens
                secret <- lookup "oauth_token_secret" accessTokens
                userId <- lookup "user_id" accessTokens
                screenName <- lookup "screen_name" accessTokens
                pure (token, secret, userId, screenName)
          case maybeAccessTokens of
            Nothing -> do
              let resp =
                    responseLBS
                      status404
                      []
                      "<p>in twitter callback, access tokens not found in getAccessToken call</p>"
              pure $ WPRResponse resp
            Just (token, secret, twitterUserId, screenName) -> do
              let tokenText = decodeUtf8With lenientDecode token
                  secretText = decodeUtf8With lenientDecode secret
                  twitterUserIdText = decodeUtf8With lenientDecode twitterUserId
                  screenNameText = decodeUtf8With lenientDecode screenName
              userId <-
                TonaDb.run $ do
                  maybeTwitterUser <- getBy $ UniqueUserId twitterUserIdText
                  case maybeTwitterUser of
                    -- This is the first time the twitter user has logged in.
                    Nothing -> do
                      time <- liftIO getCurrentTime
                      userKey <- insert $ User time
                      insert_ $
                        TwitterUser tokenText secretText twitterUserIdText screenNameText userKey
                      pure userKey
                    -- The user has already logged in before and their information exists in the database.
                    Just (Entity _ TwitterUser{twitterUserUserId}) ->
                      -- TODO: Should the user's oauth_token and oauth_token_secret be updated in the database?
                      pure twitterUserUserId
              rawCookie <- createCookie userId
              url <- readerConf redirAfterLoginUrl
              let byteStringUrl = encodeUtf8 url
                  resp =
                    responseLBS
                      status302
                      [(hLocation, byteStringUrl), (hSetCookie, rawCookie)]
                      mempty
              pure $ WPRResponse resp

createCookie :: Key User -> Tona ByteString
createCookie userKey = do
  key <- readerShared sessionKey
  let byteStringUserKey = userKeyToByteString userKey
  encryptedUserKey <- liftIO $ encryptIO key byteStringUserKey
  let setCookie =
        defaultGGSetCookie
          { setCookieValue = encryptedUserKey
          }
      rawSetCookie = toStrictByteString $ renderSetCookie setCookie
  pure rawSetCookie

deleteCookie :: ByteString
deleteCookie =
  let setCookie =
        defaultGGSetCookie
          { setCookieValue = ""
          }
      rawSetCookie = toStrictByteString $ renderSetCookie setCookie
  in rawSetCookie

defaultGGSetCookie :: SetCookie
defaultGGSetCookie =
  defaultSetCookie
    { setCookieName = "_GG_SESSION"
    , setCookiePath = Just "/"
    , setCookieMaxAge = Just $ 60 * 60 * 24 * 3650 -- 10 years
    , setCookieHttpOnly = True
    }

data HandleProxyErr
  = HandleProxyNoCookies
  | HandleProxyNoSessCookie

handleProxy :: Request -> Tona WaiProxyResponse
handleProxy req = do
  key <- readerShared sessionKey
  let oldReqHeaders = requestHeaders req
      oldReqHeadersWithoutUserId = filter (\(header, _) -> header /= "X-UserId") oldReqHeaders
      maybeCookies = parseCookies <$> lookup hCookie oldReqHeadersWithoutUserId
  $(logDebug) $ "handleProxy, maybeCookies: " <> tshow maybeCookies
  eitherResp <-
    runExceptT $ do
      cookies <- fromMaybeM (throwError HandleProxyNoCookies) maybeCookies
      let maybeSessionCookie = lookup "_GG_SESSION" cookies
      $(logDebug) $ "handleProxy, maybeSessionCookie: " <> tshow maybeSessionCookie
      sessionCookie <- fromMaybeM (throwError HandleProxyNoSessCookie) maybeSessionCookie
      $(logDebug) $ "handleProxy, sessionCookie: " <> tshow sessionCookie
      $(logDebug) $ "handleProxy, rawUserKey: " <> tshow (decrypt key sessionCookie)
      let maybeUserKey = do
            rawUserKey <- decrypt key sessionCookie
            textUserKey <- either (const Nothing) Just $ decodeUtf8' rawUserKey
            int64UserKey <- readMaybe $ unpack textUserKey
            pure $ toSqlKey int64UserKey
      $(logDebug) $ "handleProxy, maybeUserKey: " <> tshow maybeUserKey
      case maybeUserKey of
        Nothing -> do
          $(logDebug) $ "handleProxy, got no user key"
          let withoutSessionCookie = filter (\(name, _) -> name /= "_GG_SESSION") cookies
              oldReqHeadersWithoutCookie =
                filter (\(header, _) -> header /= hCookie) oldReqHeadersWithoutUserId
              newCookieHeader =
                (hCookie, toStrictByteString (renderCookies withoutSessionCookie))
              newHeaders = newCookieHeader : oldReqHeadersWithoutCookie
              newReq = req { requestHeaders = newHeaders }
          $(logDebug) $ "handleProxy, withoutSessionCookie: " <> tshow withoutSessionCookie
          $(logDebug) $ "handleProxy, oldReqHeadersWithoutCookie: " <> tshow oldReqHeadersWithoutCookie
          $(logDebug) $ "handleProxy, newCookieHeader: " <> tshow newCookieHeader
          $(logDebug) $ "handleProxy, newHeaders: " <> tshow newHeaders
          $(logDebug) $ "handleProxy, newReq: " <> tshow newReq
          pure $ WPRModifiedRequest newReq (ProxyDest "localhost" 8000)
        Just userKey -> do
          let withoutSessionCookie =
                filter (\(name, _) -> name /= "_GG_SESSION") cookies
              oldReqHeadersWithoutCookie =
                filter
                  (\(header, _) -> header /= hCookie)
                  oldReqHeadersWithoutUserId
              newCookieHeader =
                case withoutSessionCookie of
                  [] -> []
                  (_:_) ->
                    [ (hCookie
                      , toStrictByteString (renderCookies withoutSessionCookie)
                      )
                    ]
              newHeaders =
                [("X-UserId", userKeyToByteString userKey)] <>
                newCookieHeader <>
                oldReqHeadersWithoutCookie
              newReq = req { requestHeaders = newHeaders }
          $(logDebug) $ "handleProxy, userKey: " <> tshow userKey
          $(logDebug) $ "handleProxy, oldReqHeadersWithoutCookie: " <> tshow oldReqHeadersWithoutCookie
          $(logDebug) $ "handleProxy, newCookieHeader: " <> tshow newCookieHeader
          $(logDebug) $ "handleProxy, nweHeaders: " <> tshow newHeaders
          $(logDebug) $ "handleProxy, newReq: " <> tshow newReq
          pure $ WPRModifiedRequest newReq (ProxyDest "localhost" 8000)
  case eitherResp of
    Left HandleProxyNoCookies -> do
      let newReq = req { requestHeaders = oldReqHeadersWithoutUserId }
      pure $ WPRModifiedRequest newReq (ProxyDest "localhost" 8000)
    Left HandleProxyNoSessCookie -> do
      let newReq = req { requestHeaders = oldReqHeadersWithoutUserId }
      pure $ WPRModifiedRequest newReq (ProxyDest "localhost" 8000)
    Right resp -> pure resp

noUploadedFilesBackend :: Applicative m => a -> b -> n c -> m ()
noUploadedFilesBackend _ _ _ = pure ()

data EmailRegErr
  = EmailRegEmailAlreadyExists
  | EmailRegEmailInvalid
  | EmailRegNoEmailParam
  | EmailRegNoPassParam

handleEmailRegister :: Request -> Tona WaiProxyResponse
handleEmailRegister req = do
  let reqBodyOpts =
        setMaxRequestNumFiles 0 $ defaultParseRequestBodyOptions
  (params, _) <- liftIO $ parseRequestBodyEx reqBodyOpts noUploadedFilesBackend req
  let maybeEmail = lookup "email" params
      maybePass = lookup "password" params
  eitherResp <-
    runExceptT $ do
      byteStringEmail <- fromMaybeM (throwError EmailRegNoEmailParam) maybeEmail
      let email = decodeUtf8With lenientDecode byteStringEmail
      pass <- fromMaybeM (throwError EmailRegNoPassParam) maybePass
      when (not $ isValid byteStringEmail) $ throwError EmailRegEmailInvalid
      hashedPass <- hashPass $ decodeUtf8With lenientDecode pass
      time <- liftIO getCurrentTime
      token <- createLoginToken
      maybeLoginToken <-
        lift $
          TonaDb.run $ do
            maybeEmailEntity <- getBy $ UniqueEmail email
            case maybeEmailEntity of
              Just _emailEntity -> pure Nothing
              Nothing -> do
                userKey <- insert $ User time
                emailKey <- insert $ Email email hashedPass False userKey
                insert_ $ EmailLoginToken emailKey token time
                pure $ Just token
      LoginToken loginToken <-
        fromMaybeM (throwError EmailRegEmailAlreadyExists) maybeLoginToken
      let mail =
            simpleMail'
              (Address Nothing email)
              "foobar@example.com"
              "Confirm your email address"
              (fromStrict $ "http://localhost:3000/email/confirm?token=" <> loginToken)
      $(logDebug) $ "Sending email: " <> tshow mail
      lift $ TonaEmail.send mail
      let resp =
            responseLBS
              status302
              [(hLocation, "http://localhost:3000/")]
              mempty
      pure $ WPRResponse resp
  case eitherResp of
    Left EmailRegEmailAlreadyExists ->
      pure $ errResp status409 "email address already exists"
    Left EmailRegEmailInvalid ->
      pure $ errResp status400 "invalid email address"
    Left EmailRegNoEmailParam ->
      pure $ errResp status400 "email request body param not found"
    Left EmailRegNoPassParam ->
      pure $ errResp status400 "password request body param not found"
    Right resp -> pure resp
  where
    errResp :: Status -> Text -> WaiProxyResponse
    errResp status msg =
      let resp =
            responseLBS status [] $
              "<p>in email register, " <>
              ByteString.Lazy.fromStrict (encodeUtf8 msg) <>
              "</p>"
      in WPRResponse resp

data EmailConfRes
  = EmailConfNoEmailLoginTokenInDb
  | EmailConfNoTokenParam

handleEmailConfirm :: Request -> Tona WaiProxyResponse
handleEmailConfirm req = do
  let maybeToken = join $ lookup "token" $ queryString req
  eitherResp <-
    runExceptT $ do
      byteStringToken <- fromMaybeM (throwError EmailConfNoTokenParam) maybeToken
      let token = decodeUtf8With lenientDecode byteStringToken
      ExceptT $
        TonaDb.run $ do
          maybeLoginTokenEntity <-
            selectFirst [EmailLoginTokenLoginToken ==. LoginToken token] []
          case maybeLoginTokenEntity of
            Nothing -> pure $ Left EmailConfNoEmailLoginTokenInDb
            Just (Entity _ EmailLoginToken{emailLoginTokenEmailId}) -> do
              update emailLoginTokenEmailId [EmailVerified =. True]
              pure $ Right ()
  case eitherResp of
    Left EmailConfNoEmailLoginTokenInDb ->
      pure $ errResp status404 "no corresponding login token in db"
    Left EmailConfNoTokenParam ->
      pure $ errResp status400 "token query param not found"
    Right () -> do
      let resp =
            responseLBS status302 [(hLocation, "http://localhost:3000/")] mempty
      pure $ WPRResponse resp
  where
    errResp :: Status -> Text -> WaiProxyResponse
    errResp status msg =
      let resp =
            responseLBS status [] $
              "<p>in email confirm, " <>
              ByteString.Lazy.fromStrict (encodeUtf8 msg) <>
              "</p>"
      in WPRResponse resp

data EmailLoginErr
  = EmailLoginEmailNotFoundInDb
  | EmailLoginEmailNotVerified
  | EmailLoginNoEmailParam
  | EmailLoginNoPassParam
  | EmailLoginPassIncorrect
  | EmailLoginUserNotFoundInDb

handleEmailLogin :: Request -> Tona WaiProxyResponse
handleEmailLogin req = do
  let reqBodyOpts =
        setMaxRequestNumFiles 0 $ defaultParseRequestBodyOptions
  (params, _) <- liftIO $ parseRequestBodyEx reqBodyOpts noUploadedFilesBackend req
  let maybeEmail = lookup "email" params
      maybePass = lookup "password" params
  eitherResp <-
    runExceptT $ do
      byteStringEmail <- fromMaybeM (throwError EmailLoginNoEmailParam) maybeEmail
      let email = decodeUtf8With lenientDecode byteStringEmail
      byteStringPass <- fromMaybeM (throwError EmailLoginNoPassParam) maybePass
      let pass = decodeUtf8With lenientDecode byteStringPass
      maybeEmailEnt <- lift $ TonaDb.run $ getBy $ UniqueEmail email
      Entity _ Email{emailHashedPass, emailVerified, emailUserId} <-
        fromMaybeM (throwError EmailLoginEmailNotFoundInDb) maybeEmailEnt
      when (not emailVerified) $ throwError EmailLoginEmailNotVerified
      when (not $ checkPass pass emailHashedPass) $ throwError EmailLoginPassIncorrect
      maybeUserEnt <- lift $ TonaDb.run $ getEntity emailUserId
      Entity userKey _ <- fromMaybeM (throwError EmailLoginUserNotFoundInDb) maybeUserEnt
      rawCookie <- lift $ createCookie userKey
      url <- lift $ readerConf redirAfterLoginUrl
      let byteStringUrl = encodeUtf8 url
          resp =
            responseLBS
              status302
              [(hLocation, byteStringUrl), (hSetCookie, rawCookie)]
              mempty
      pure $ WPRResponse resp
  case eitherResp of
    Left EmailLoginEmailNotFoundInDb ->
      pure $ errResp status403 "error logging in"
    Left EmailLoginEmailNotVerified ->
      pure $ errResp status403 "email address not verified"
    Left EmailLoginNoEmailParam ->
      pure $ errResp status400 "email request body param not found"
    Left EmailLoginNoPassParam ->
      pure $ errResp status400 "password request body param not found"
    Left EmailLoginPassIncorrect ->
      pure $ errResp status403 "error logging in"
    Left EmailLoginUserNotFoundInDb ->
      pure $ errResp status500 "internal error, user not found in db"
    Right resp -> pure resp
  where
    errResp :: Status -> Text -> WaiProxyResponse
    errResp status msg =
      let resp =
            responseLBS status [] $
              "<p>in email login, " <>
              ByteString.Lazy.fromStrict (encodeUtf8 msg) <>
              "</p>"
      in WPRResponse resp

data EmailChangePassErr
  = EmailChangePassNoCookies
  | EmailChangePassNoNewPassParam
  | EmailChangePassNoOldPassParam
  | EmailChangePassNoSessCookie
  | EmailChangePassOldPassIncorrect
  | EmailChangePassSessCookieIncorrect
  | EmailChangePassUserDoesNotExist
  | EmailChangePassUserNotVerified

handleEmailChangePass :: Request -> Tona WaiProxyResponse
handleEmailChangePass req = do
  key <- readerShared sessionKey
  let reqBodyOpts =
        setMaxRequestNumFiles 0 $ defaultParseRequestBodyOptions
      oldReqHeaders = requestHeaders req
      oldReqHeadersWithoutUserId = filter (\(header, _) -> header /= "X-UserId") oldReqHeaders
      maybeCookies = parseCookies <$> lookup hCookie oldReqHeadersWithoutUserId
  (params, _) <- liftIO $ parseRequestBodyEx reqBodyOpts noUploadedFilesBackend req
  let maybeOldPass = lookup "old-pass" params
      maybeNewPass = lookup "new-pass" params
  eitherResp <-
    runExceptT $ do
      cookies <- fromMaybeM (throwError EmailChangePassNoCookies) maybeCookies
      let maybeSessionCookie = lookup "_GG_SESSION" cookies
      sessionCookie <- fromMaybeM (throwError EmailChangePassNoSessCookie) maybeSessionCookie
      let maybeUserKey = do
            rawUserKey <- decrypt key sessionCookie
            textUserKey <- either (const Nothing) Just $ decodeUtf8' rawUserKey
            int64UserKey <- readMaybe $ unpack textUserKey
            pure $ toSqlKey int64UserKey
      userKey <- fromMaybeM (throwError EmailChangePassSessCookieIncorrect) maybeUserKey
      byteStringOldPass <- fromMaybeM (throwError EmailChangePassNoOldPassParam) maybeOldPass
      byteStringNewPass <- fromMaybeM (throwError EmailChangePassNoNewPassParam) maybeNewPass
      let oldPass = decodeUtf8With lenientDecode byteStringOldPass
      let newPass = decodeUtf8With lenientDecode byteStringNewPass
      maybeEmailEnt <-
        lift $ TonaDb.run $ selectFirst [EmailUserId ==. userKey] []
      Entity emailKey emailEnt@Email{emailHashedPass, emailVerified} <-
        fromMaybeM (throwError EmailChangePassUserDoesNotExist) maybeEmailEnt
      when (not emailVerified) $
        throwError EmailChangePassUserNotVerified
      when (not $ checkPass oldPass emailHashedPass) $ throwError EmailChangePassOldPassIncorrect
      newHashedPass <- hashPass newPass
      let newEmailEnt = emailEnt { emailHashedPass = newHashedPass }
      lift $ TonaDb.run $ repsert emailKey newEmailEnt
      url <- lift $ readerConf redirAfterLoginUrl
      let byteStringUrl = encodeUtf8 url
          resp = responseLBS status302 [(hLocation, byteStringUrl)] mempty
      pure $ WPRResponse resp
  case eitherResp of
    Left EmailChangePassNoCookies ->
      pure $ errResp status400 "no cookies found"
    Left EmailChangePassNoNewPassParam ->
      pure $ errResp status400 "new-pass request body param not found"
    Left EmailChangePassNoOldPassParam ->
      pure $ errResp status400 "old-pass request body param not found"
    Left EmailChangePassNoSessCookie ->
      pure $ errResp status400 "the _GG_SESSION cookie is not found"
    Left EmailChangePassOldPassIncorrect ->
      pure $ errResp status403 "old-pass is incorrect"
    Left EmailChangePassSessCookieIncorrect ->
      pure $ errResp status400 "the _GG_SESSION cookie is malformed"
    Left EmailChangePassUserDoesNotExist ->
      pure $ errResp status500 "the user in question does not exist"
    Left EmailChangePassUserNotVerified ->
      pure $ errResp status403 "the user is not yet verified"
    Right resp -> pure resp
  where
    errResp :: Status -> Text -> WaiProxyResponse
    errResp status msg =
      let resp =
            responseLBS status [] $
              "<p>in email change pass, " <>
              ByteString.Lazy.fromStrict (encodeUtf8 msg) <>
              "</p>"
      in WPRResponse resp

data EmailResetPassSendEmailErr
  = EmailResetPassSendEmailEmailDoesNotExist
  | EmailResetPassSendEmailNoEmailParam
  | EmailResetPassSendEmailNoNextParam

handleEmailResetPassSendEmail :: Request -> Tona WaiProxyResponse
handleEmailResetPassSendEmail req = do
  let reqBodyOpts =
        setMaxRequestNumFiles 0 $ defaultParseRequestBodyOptions
  (params, _) <-
    liftIO $ parseRequestBodyEx reqBodyOpts noUploadedFilesBackend req
  let maybeEmail = lookup "email" params
      maybeNext = lookup "next" params
  eitherResp <-
    runExceptT $ do
      byteStringEmail <-
        fromMaybeM (throwError EmailResetPassSendEmailNoEmailParam) maybeEmail
      let email = decodeUtf8With lenientDecode byteStringEmail
      next <-
        fromMaybeM (throwError EmailResetPassSendEmailNoNextParam) maybeNext
      time <- liftIO getCurrentTime
      token <- createLoginToken
      maybeLoginToken <-
        lift $
          TonaDb.run $ do
            maybeEmailEntity <- getBy $ UniqueEmail email
            case maybeEmailEntity of
              Nothing -> pure Nothing
              Just (Entity emailKey _) -> do
                insert_ $ EmailLoginToken emailKey token time
                pure $ Just token
      LoginToken loginToken <-
        fromMaybeM
          (throwError EmailResetPassSendEmailEmailDoesNotExist)
          maybeLoginToken
      let mail =
            simpleMail'
              (Address Nothing email)
              "foobar@example.com"
              "Confirm your email address"
              (fromStrict $
                "http://localhost:3000/email/reset-password-login-with-token?token=" <> loginToken <>
                "&next=" <> decodeUtf8With lenientDecode (urlEncode True next)
              )
      $(logDebug) $ "Sending email: " <> tshow mail
      lift $ TonaEmail.send mail
      let resp =
            responseLBS
              status302
              [(hLocation, "http://localhost:3000/")]
              mempty
      pure $ WPRResponse resp
  case eitherResp of
    Left EmailResetPassSendEmailEmailDoesNotExist ->
      pure $ errResp status409 "email address not in database"
    Left EmailResetPassSendEmailNoEmailParam ->
      pure $ errResp status400 "email request body param not found"
    Left EmailResetPassSendEmailNoNextParam ->
      pure $ errResp status400 "next request body param not found"
    Right resp -> pure resp
  where
    errResp :: Status -> Text -> WaiProxyResponse
    errResp status msg =
      let resp =
            responseLBS status [] $
              "<p>in email reset pass send email, " <>
              ByteString.Lazy.fromStrict (encodeUtf8 msg) <>
              "</p>"
      in WPRResponse resp

data EmailResetPassTokenRes
  = EmailResetPassTokenNoEmailInDb
  | EmailResetPassTokenNoTokenInDb
  | EmailResetPassTokenNoUserInDb
  | EmailResetPassTokenNoNextParam
  | EmailResetPassTokenNoTokenParam

handleEmailResetPassToken :: Request -> Tona WaiProxyResponse
handleEmailResetPassToken req = do
  let maybeToken = join $ lookup "token" $ queryString req
      maybeNext = join $ lookup "next" $ queryString req
  eitherResp <-
    runExceptT $ do
      byteStringToken <- fromMaybeM (throwError EmailResetPassTokenNoTokenParam) maybeToken
      next <- fromMaybeM (throwError EmailResetPassTokenNoNextParam) maybeNext
      let token = decodeUtf8With lenientDecode byteStringToken
      maybeUserEnt <-
        ExceptT $
          TonaDb.run $ do
            maybeLoginTokenEntity <-
              selectFirst [EmailLoginTokenLoginToken ==. LoginToken token] []
            case maybeLoginTokenEntity of
              Nothing -> pure $ Left EmailResetPassTokenNoTokenInDb
              Just (Entity _ EmailLoginToken{emailLoginTokenEmailId}) -> do
                maybeEmail <- get emailLoginTokenEmailId
                case maybeEmail of
                  Nothing -> pure $ Left EmailResetPassTokenNoEmailInDb
                  Just Email{emailUserId} -> Right <$> getEntity emailUserId
      Entity userKey _ <- fromMaybeM (throwError EmailResetPassTokenNoUserInDb) maybeUserEnt
      rawCookie <- lift $ createCookie userKey
      let resp =
            responseLBS
              status302
              [(hLocation, next), (hSetCookie, rawCookie)]
              mempty
      pure $ WPRResponse resp
  case eitherResp of
    Left EmailResetPassTokenNoEmailInDb ->
      pure $ errResp status404 "no email in db"
    Left EmailResetPassTokenNoTokenInDb ->
      pure $ errResp status404 "no token in db"
    Left EmailResetPassTokenNoUserInDb ->
      pure $ errResp status404 "no user in db"
    Left EmailResetPassTokenNoNextParam ->
      pure $ errResp status400 "next query param not found"
    Left EmailResetPassTokenNoTokenParam ->
      pure $ errResp status400 "token query param not found"
    Right resp -> pure resp
  where
    errResp :: Status -> Text -> WaiProxyResponse
    errResp status msg =
      let resp =
            responseLBS status [] $
              "<p>in email reset pass login with token, " <>
              ByteString.Lazy.fromStrict (encodeUtf8 msg) <>
              "</p>"
      in WPRResponse resp

data EmailResetPassErr
  = EmailResetPassNoCookies
  | EmailResetPassNoNewPassParam
  | EmailResetPassNoSessCookie
  | EmailResetPassSessCookieIncorrect
  | EmailResetPassUserDoesNotExist

handleEmailResetPass :: Request -> Tona WaiProxyResponse
handleEmailResetPass req = do
  key <- readerShared sessionKey
  let reqBodyOpts =
        setMaxRequestNumFiles 0 $ defaultParseRequestBodyOptions
      oldReqHeaders = requestHeaders req
      oldReqHeadersWithoutUserId = filter (\(header, _) -> header /= "X-UserId") oldReqHeaders
      maybeCookies = parseCookies <$> lookup hCookie oldReqHeadersWithoutUserId
  (params, _) <- liftIO $ parseRequestBodyEx reqBodyOpts noUploadedFilesBackend req
  let maybeNewPass = lookup "new-pass" params
  eitherResp <-
    runExceptT $ do
      cookies <- fromMaybeM (throwError EmailResetPassNoCookies) maybeCookies
      let maybeSessionCookie = lookup "_GG_SESSION" cookies
      sessionCookie <- fromMaybeM (throwError EmailResetPassNoSessCookie) maybeSessionCookie
      let maybeUserKey = do
            rawUserKey <- decrypt key sessionCookie
            textUserKey <- either (const Nothing) Just $ decodeUtf8' rawUserKey
            int64UserKey <- readMaybe $ unpack textUserKey
            pure $ toSqlKey int64UserKey
      userKey <- fromMaybeM (throwError EmailResetPassSessCookieIncorrect) maybeUserKey
      byteStringNewPass <- fromMaybeM (throwError EmailResetPassNoNewPassParam) maybeNewPass
      let newPass = decodeUtf8With lenientDecode byteStringNewPass
      newHashedPass <- hashPass newPass
      ExceptT $
        TonaDb.run $ do
          maybeEmailEnt <- selectFirst [EmailUserId ==. userKey] []
          case maybeEmailEnt of
            Nothing -> pure $ Left EmailResetPassUserDoesNotExist
            Just (Entity emailKey email) -> do
              let newEmail = email { emailHashedPass = newHashedPass }
              repsert emailKey newEmail
              pure $ Right ()
      url <- lift $ readerConf redirAfterLoginUrl
      let byteStringUrl = encodeUtf8 url
          resp = responseLBS status302 [(hLocation, byteStringUrl)] mempty
      pure $ WPRResponse resp
  case eitherResp of
    Left EmailResetPassNoCookies ->
      pure $ errResp status400 "no cookies found"
    Left EmailResetPassNoNewPassParam ->
      pure $ errResp status400 "new-pass request body param not found"
    Left EmailResetPassNoSessCookie ->
      pure $ errResp status400 "the _GG_SESSION cookie is not found"
    Left EmailResetPassSessCookieIncorrect ->
      pure $ errResp status400 "the _GG_SESSION cookie is malformed"
    Left EmailResetPassUserDoesNotExist ->
      pure $ errResp status500 "the user in question does not exist"
    Right resp -> pure resp
  where
    errResp :: Status -> Text -> WaiProxyResponse
    errResp status msg =
      let resp =
            responseLBS status [] $
              "<p>in email reset pass, " <>
              ByteString.Lazy.fromStrict (encodeUtf8 msg) <>
              "</p>"
      in WPRResponse resp

handleLogout :: Request -> Tona WaiProxyResponse
handleLogout req = do
  let maybeNext = join $ lookup "next" $ queryString req
  url <- maybe (encodeUtf8 <$> readerConf redirAfterLoginUrl) pure maybeNext
  let resp =
        responseLBS
          status302
          [(hLocation, url), (hSetCookie, deleteCookie)]
          mempty
  pure $ WPRResponse resp

toStrictByteString :: Builder -> ByteString
toStrictByteString = toStrict . toLazyByteString

userKeyToByteString :: Key User -> ByteString
userKeyToByteString = encodeUtf8 . tshow . fromSqlKey

router :: Request -> Tona WaiProxyResponse
router req = do
  let reqPath = pathInfo req
  case reqPath of
    "twitter":"callback":_ -> handleTwitterCallback req
    "twitter":"login":_ -> handleTwitterLogin req
    "email":"register":_ -> handleEmailRegister req
    "email":"confirm":_ -> handleEmailConfirm req
    "email":"login":_ -> handleEmailLogin req
    "email":"change-password":_ -> handleEmailChangePass req
    "email":"reset-password-send-email":_ -> handleEmailResetPassSendEmail req
    "email":"reset-password-login-with-token":_ -> handleEmailResetPassToken req
    "email":"reset-password":_ -> handleEmailResetPass req
    "logout":_ -> handleLogout req
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
