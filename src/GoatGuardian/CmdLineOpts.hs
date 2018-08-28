{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module GoatGuardian.CmdLineOpts where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import Data.Semigroup (Semigroup, (<>))
import Data.String (IsString)
import Options.Applicative (Parser, (<**>), execParser, fullDesc, header, help, helper, info, long, short, switch)
import TonaParser (FromEnv(..), (.||), argLong, env, envVar)
import Web.ClientSession (Key, initKey)

data RawSessionKey = RawSessionKey { unRawSessionKey :: ByteString }

initRawSessKey :: RawSessionKey -> Either String Key
initRawSessKey (RawSessionKey rawSessKey) = do
  decodedSessKey <- Base64.decode rawSessKey
  initKey decodedSessKey

initRawSessKeyOrFail :: RawSessionKey -> IO Key
initRawSessKeyOrFail rawSessKey = do
  let msg = "Could not decode the GG_SESSION_KEY.\n\n" <> createNewKeyMsg
  either (fail msg) pure (initRawSessKey rawSessKey)

createNewKeyMsg :: (IsString s, Semigroup s) => s
createNewKeyMsg =
  "A new key can be created by running goat-guardian like the following:\n\n" <>
  "  $ goat-guardian --generate-session-key"

instance Show RawSessionKey where
  show _ = "(raw session key)"

instance FromEnv RawSessionKey where
  fromEnv = do
    b64SessKey <- env (envVar "GG_SESSION_KEY" .|| argLong "session-key")
    let eitherB64SessKey = Base64.decode b64SessKey
    case eitherB64SessKey of
      Left _ ->
        error $
          "Could not base64-decode the GG_SESSION_KEY.\n\n" <> createNewKeyMsg
      Right sessKey -> do
        if ByteString.length sessKey /= 96
          then
            error $
              "The base64-decoded GG_SESSION_KEY is not exactly 96 bytes.\n\n" <>
              createNewKeyMsg
          else pure $ RawSessionKey b64SessKey

data CmdLineOpts = CmdLineOpts
  { genSessKey :: Maybe GenSessKey
  }

data GenSessKey = GenSessKey

genSessKeyParser :: Parser (Maybe GenSessKey)
genSessKeyParser = do
  res <-
    switch $
      long "generate-session-key" <>
      short 'g' <>
      help "Generate a new session key to be used on the command line."
  pure $
    if res
      then Just GenSessKey
      else Nothing

options :: Parser CmdLineOpts
options = CmdLineOpts <$> genSessKeyParser

parseCmdLineOpts :: IO CmdLineOpts
parseCmdLineOpts =
  execParser $
    info
      (options <**> helper)
      (fullDesc <> header "goat-guardian - a reverse-proxy authentication server")
