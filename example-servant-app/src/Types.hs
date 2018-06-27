{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Types where

import Data.Int (Int64)
import Database.Persist.Sqlite (PersistField, PersistFieldSql)
import Web.HttpApiData (FromHttpApiData)

newtype UserId = UserId
  { unUserId :: Int64
  } deriving (Eq, FromHttpApiData, Num, PersistField, PersistFieldSql, Show)
