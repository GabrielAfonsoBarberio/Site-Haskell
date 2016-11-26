{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE QuasiQuotes          #-}

module Application where

import Foundation
import Yesod

-- AQUI MORAM OS HANDLERS
-- import Add
-- PARA CADA NOVO GRUPO DE HANDLERS, CRIAR UM AQUIVO
-- DE HANDLER NOVO E IMPORTAR AQUI
import Objetos
import Pessoas
------------------
mkYesodDispatch "App" resourcesApp

getHomeR ::Handler Html
getHomeR = defaultLayout $ do

    toWidget [lucius|
    h1{
        color:red;
    }
    |]
    [whamlet|
        <h1> 
        <ul>
            <li> <a href=@{PessR}> Cadastrar novos clientes
            <li> <a href=@{ObjetoR}> Cadastro
            <li> <a href=@{ListPessR}> Clientes
            <li> <a href=@{ListObjetoR}> Inventario
    |]