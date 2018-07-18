{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module GoatGuardian where

import Control.Monad (join)
import Control.Monad.Catch (try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Lazy.Builder (Builder, toLazyByteString)
import Data.List (find)
import Data.Semigroup ((<>))
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8', decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy (fromStrict)
import qualified Data.Text.IO as Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.Persist.Sql (Entity(..), Key, (==.), deleteWhere, fromSqlKey, getBy, insert, insert_, selectFirst, toSqlKey)
import Database.Persist.TH (mkMigrate, mkPersist, mpsGenerateLenses, persistLowerCase, share, sqlSettings)
import Network.HTTP.Conduit (HttpException, Manager, newManager, tlsManagerSettings)
import Network.HTTP.ReverseProxy
import Network.HTTP.Types.Header (hCookie, hLocation, hSetCookie)
import Network.HTTP.Types.Status (status302, status403, status404, status500)
import Network.Wai (Request, Response, ResponseReceived, pathInfo, queryString, requestHeaders, responseLBS)
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Parse
import System.Envy
import Text.Email.Validate (isValid)
import Text.Read (readMaybe)
import Tonatona (Plug(..), TonaM, readerConf, readerShared)
import qualified Tonatona as Tona
import Tonatona.Db.Sqlite (TonaDbConfig, TonaDbSqlShared)
import qualified Tonatona.Db.Sqlite as TonaDb
import Tonatona.Email.Sendmail (Address(Address), simpleMail')
import qualified Tonatona.Email.Sendmail as TonaEmail
import Tonatona.Logger (TonaLoggerShared, logDebug, stdoutLogger)
import qualified Tonatona.Logger as TonaLogger
import Web.Authenticate.OAuth (Credential(..), authorizeUrl, getAccessToken, getTemporaryCredential)
import Web.ClientSession (decrypt, encryptIO, randomKey)
import qualified Web.ClientSession as ClientSession
import Web.Cookie (defaultSetCookie, parseCookies, renderCookies, renderSetCookie, setCookieHttpOnly, setCookieMaxAge, setCookieName, setCookiePath, setCookieValue)
import Web.Twitter.Conduit (OAuth(..), twitterOAuth)

import GoatGuardian.CmdLineOpts (CmdLineOpts(..), RawSessionKey(..), initRawSessKeyOrFail, parseCmdLineOpts)
import GoatGuardian.Password (hashPass)
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
    email      EmailId
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
    TwitterConfig
      <$> envMaybe "GG_TWITTER_OAUTH_CALLBACK_URL" .!= "http://localhost:3000/twitter/callback"
      <*> env "GG_TWITTER_OAUTH_KEY"
      <*> env "GG_TWITTER_OAUTH_SECRET"

data Config = Config
  { tonaDb :: TonaDb.Config
  , twitterConfig :: TwitterConfig
  , rawSessionKey :: RawSessionKey
  , redirAfterLoginUrl :: Text
  }
  deriving (Show)

instance FromEnv Config where
  fromEnv =
    Config
      <$> fromEnv
      <*> fromEnv
      <*> fromEnv
      <*> envMaybe "GG_REDIR_AFTER_LOGIN_URL" .!= "http://localhost:3000"

instance TonaDbConfig Config where
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

instance TonaDbSqlShared Shared where
  shared = tonaDb

instance TonaLoggerShared Shared where
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
        liftIO . run 3000 . logStdoutDev $ app conf shared

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
        defaultSetCookie
          { setCookieName = "_GG_SESSION"
          , setCookieValue = encryptedUserKey
          , setCookiePath = Just "/"
          , setCookieMaxAge = Just $ 60 * 60 * 24 * 3650 -- 10 years
          , setCookieHttpOnly = True
          }
      rawSetCookie = toStrictByteString $ renderSetCookie setCookie
  pure rawSetCookie


handleProxy :: Request -> Tona WaiProxyResponse
handleProxy req = do
  key <- readerShared sessionKey
  let oldReqHeaders = requestHeaders req
      oldReqHeadersWithoutUserId = filter (\(header, _) -> header /= "X-UserId") oldReqHeaders
      maybeRawCookies = find (\(header, _) -> header == hCookie) oldReqHeadersWithoutUserId
      maybeCookies = parseCookies . snd <$> maybeRawCookies
  $(logDebug) $ "handleProxy, maybeCookies: " <> tshow maybeCookies
  case maybeCookies of
    Nothing -> do
      let newReq = req { requestHeaders = oldReqHeadersWithoutUserId }
      pure $ WPRModifiedRequest newReq (ProxyDest "localhost" 8000)
    Just cookies -> do
      let maybeSessionCookie = find (\(name, _) -> name == "_GG_SESSION") cookies
      $(logDebug) $ "handleProxy, maybeSessionCookie: " <> tshow maybeSessionCookie
      case maybeSessionCookie of
        Nothing -> do
          let newReq = req { requestHeaders = oldReqHeadersWithoutUserId }
          pure $ WPRModifiedRequest newReq (ProxyDest "localhost" 8000)
        Just (_, sessionCookie) -> do
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
              let withoutSessionCookie = filter (\(name, _) -> name /= "_GG_SESSION") cookies
                  oldReqHeadersWithoutCookie =
                    filter (\(header, _) -> header /= hCookie) oldReqHeadersWithoutCookie
                  newCookieHeader =
                    (hCookie, toStrictByteString (renderCookies withoutSessionCookie))
                  newHeaders = newCookieHeader : oldReqHeadersWithoutCookie
                  newReq = req { requestHeaders = newHeaders }
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

noUploadedFilesBackend :: Applicative m => a -> b -> n c -> m ()
noUploadedFilesBackend _ _ _ = pure ()

handleEmailRegister :: Request -> Tona WaiProxyResponse
handleEmailRegister req = do
  let reqBodyOpts =
        setMaxRequestNumFiles 0 $ defaultParseRequestBodyOptions
  (params, _) <- liftIO $ parseRequestBodyEx reqBodyOpts noUploadedFilesBackend req
  let maybeEmail = find (\(param, _) -> param == "email") params
      maybePass = find (\(param, _) -> param == "password") params
  case maybeEmail of
    Nothing -> undefined
    Just (_, byteStringEmail) -> do
      let email = decodeUtf8With lenientDecode byteStringEmail
      case maybePass of
        Nothing -> undefined
        Just (_, pass) ->
          if not (isValid byteStringEmail)
            then undefined
            else do
              hashedPass <- hashPass (decodeUtf8With lenientDecode pass)
              time <- liftIO getCurrentTime
              token <- createLoginToken
              maybeLoginToken <- TonaDb.run $ do
                maybeEmailEntity <- getBy $ UniqueEmail email
                case maybeEmailEntity of
                  Just _emailEntity -> pure Nothing
                  Nothing -> do
                    userKey <- insert $ User time
                    emailKey <- insert $ Email email hashedPass False userKey
                    _emailLoginTokenKey <-
                      insert $ EmailLoginToken emailKey token time
                    pure $ Just token
              case maybeLoginToken of
                Nothing -> undefined
                Just (LoginToken loginToken) -> do
                  let mail =
                        simpleMail'
                          (Address Nothing email)
                          "foobar@example.com"
                          "Confirm your email address"
                          (fromStrict $ "http://localhost:3000/email/confirm?token=" <> loginToken)
                  TonaEmail.send mail
                  let resp =
                        responseLBS
                          status302
                          [(hLocation, "http://localhost:3000/")]
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
