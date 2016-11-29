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

data App = App {getStatic :: Static, connPool :: ConnectionPool }

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

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App where
    authRoute _ = Just LoginR
    
    isAuthorized LoginR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized ListAdminR _ = return Authorized
    isAuthorized ListInvR _ = return Authorized
    isAuthorized ListClienteR _ = return Authorized
    isAuthorized ListRelatorioR _ = return Authorized
    isAuthorized _ _ = estaAutenticado

estaAutenticado :: Handler AuthResult
estaAutenticado = do
   msu <- lookupSession "_ID"
   case msu of
       Just _ -> return Authorized
       Nothing -> return AuthenticationRequired

instance YesodPersist App where
   type YesodPersistBackend App = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage