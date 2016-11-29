{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable, EmptyDataDecls,
             GeneralizedNewtypeDeriving, ViewPatterns, FlexibleInstances #-}
module Foundation where

import Yesod
import Yesod.Static
import Data.Text
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool)

data Sitio = Sitio {getStatic :: Static, connPool :: ConnectionPool }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Inventario
    nome             Text
    tipo             Text
    disponibilidade  Text

Cliente
    nome            Text
    cpf             Text
    endereco        Text
    cidade          Text
    estado          Text
    telefone        Text

Funcionario
    nome         Text
    email        Text
    senha        Text
    UniqueEmail  email

Relatorio
    clienteid       ClienteId
    itemid          InventarioId
    funcionarioid   FuncionarioId
    concluido       Text
    UniqueRelato    clienteid itemid funcionarioid
    deriving Show
    
|]

staticFiles "static"

mkYesodData "Sitio" $(parseRoutesFile "routes")

instance Yesod Sitio

instance YesodPersist Sitio where
   type YesodPersistBackend Sitio = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Sitio FormMessage where
    renderMessage _ _ = defaultFormMessage