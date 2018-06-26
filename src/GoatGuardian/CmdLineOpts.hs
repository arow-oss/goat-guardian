{-# LANGUAGE ApplicativeDo #-}

module GoatGuardian.CmdLineOpts where

import Control.Monad.Except (throwError)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import Data.Semigroup ((<>))
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.IO as Text
import Options.Applicative (Parser, (<**>), execParser, fullDesc, header, help, helper, info, long, short, switch)
import qualified Options.Applicative as OptParse
import System.Envy (FromEnv(..), env)

data RawSessionKey = RawSessionKey { unRawSessionKey :: ByteString }

instance Show RawSessionKey where
  show _ = "(raw session key)"

instance FromEnv RawSessionKey where
  fromEnv = do
    b64SessKey <- env "GG_SESSION_KEY"
    let eitherB64SessKey = Base64.decode b64SessKey
        createNewKeyMsg =
          "A new key can be created by running goat-guardian like the following:\n\n" <>
          "  $ goat-guardian --generate-session-key"
    case eitherB64SessKey of
      Left err ->
        throwError $
          "Could not base64-decode the GG_SESSION_KEY.\n\n" <> createNewKeyMsg
      Right sessKey -> do
        if ByteString.length sessKey /= 96
          then
            throwError $
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
