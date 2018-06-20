module Lib
    ( someFunc
    ) where

import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Servant

someFunc :: IO ()
someFunc = putStrLn "someFunc"
