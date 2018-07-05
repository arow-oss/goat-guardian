{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GoatGuardian.Types where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text, pack)
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import System.Random (randomRIO)
import Web.HttpApiData (FromHttpApiData)

newtype LoginToken = LoginToken
  { unLoginToken :: Text
  } deriving (Eq, FromHttpApiData, PersistField, PersistFieldSql, Read, Show)

createLoginToken :: MonadIO m => m LoginToken
createLoginToken = do
  loginTokenStr <- liftIO $ replicateM 48 $ randomRIO ('a', 'z')
  pure $ LoginToken (pack loginTokenStr)
