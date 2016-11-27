{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE QuasiQuotes          #-}

module Application where

import Foundation
import Yesod
import Handlers.Inventario
import Handlers.Pessoas
import Handlers.Login

-- AQUI MORAM OS HANDLERS
-- import Add
-- PARA CADA NOVO GRUPO DE HANDLERS, CRIAR UM AQUIVO
-- DE HANDLER NOVO E IMPORTAR AQUI

import Inventario
import Pessoas
------------------
mkYesodDispatch "App" resourcesApp


getHomeR :: Handler Html
getHomeR = defaultLayout $ do
     sess <- lookupSession "_ID"
     [whamlet|
         <h1> _{}
         <h2> _{MsgBye}
         <button> _{Login} </button>
         $maybe _ <- sess
             <form action=@{LogoutR} method=post>
                 <input type="submit" value="Logout">
     |]
     
getPerfilR ::Handler Html
getPerfilR = defaultLayout $ do

toWidget [lucius|
    h1{
        color:red;
        size:20
    }
    |]
    [whamlet|
        <h1> 
        <ul>
            <li> <a href=@{LoginR}> Login
            <li> <a href=@{ListInvR}> Inventario
            <li> <a href=@{ListPessR}> Clientes
    |]

getPerfilR ::Handler Html
getPerfilR = defaultLayout $ do

    toWidget [lucius|
    h1{
        color:red;
    }
    |]
    [whamlet|
        <h1> 
        <ul>
            <li> <a href=@{PessR}> Cadastrar novos clientes
            <li> <a href=@{ListInvR}> Inventario
            <li> <a href=@{ListPessR}> Clientes
    |]