{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable, EmptyDataDecls,
             GeneralizedNewtypeDeriving, ViewPatterns, FlexibleInstances #-}
module Foundation where

import Yesod
import Data.Text
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool)

data App = App {connPool :: ConnectionPool }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Objetos
    objetoid
    nome    Text
    sigla   Text

Pessoas
    pessoaid    Text
    nome        Text
    endereço    Text
    cidade      Text
    estado      Text
    telefone    Text
    celular     Text

Funcionarios
    usuarioid   Text
    nome        Text
    email       Text
    senha       Text

Relatorio
    pessoasid ProfessorId
    objetoid CursoId
    UniquePesObj pessoasid objetoid
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