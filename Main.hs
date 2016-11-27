{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}

module Main where
import Foundation
import Application
import Yesod
import Yesod.Static
import Control.Applicative
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Text
import Database.Persist.Postgresql

connStr :: ConnectionString
connStr = "dbname=d149c6o4rvq6s5 host=ec2-107-22-238-96.compute-1.amazonaws.com user=btnoexaopwuhzh password=P1abqUfrm7NgFyrqfEpa_sS9gQ port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool 
       warp 8080 (App pool)