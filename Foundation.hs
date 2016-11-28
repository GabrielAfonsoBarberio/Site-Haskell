{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable, EmptyDataDecls,
             GeneralizedNewtypeDeriving, ViewPatterns, FlexibleInstances #-}
module Foundation where

import Yesod
import Data.Bool
import Data.Text
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool)

data App = App {connPool :: ConnectionPool }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Inventario
    nome             Text
    tipo             Text
    disponibilidade  Int

Cliente
    nome            Text
    cpf             Text
    endereco        Text
    cidade          Text
    estado          Text
    telefone        Text

Funcionario
    nome         Text
    posicao      Text
    email        Text
    senha        Text

Relatorio
    clienteid       ClienteId
    itemid          ItemId
    funcionarioid   FuncionarioId
    concluido       Int
    UniqueRelato    clienteid itemid funcionarioid
    deriving Show
    
|]

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App

instance YesodPersist App where
   type YesodPersistBackend App = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage