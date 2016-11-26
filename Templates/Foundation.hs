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
    inventarioid     Text
    nome             Text
    tipo             Text
    disponibilidade  Bool

Pessoas
    pessoasid       Text
    nome            Text
    cpf             Text
    endereco        Text
    cidade          Text
    estado          Text
    telefone        Text

Funcionarios
    usuariosid   Text
    nome         Text
    email        Text
    password     Text

Relatorio
    pessoasid       PessoasId
    inventarioid    InventarioId
    usuariosid      UsuariosId
    disponibilidade Bool
    UniquePesInv    pessoasid objetoid usuariosid
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