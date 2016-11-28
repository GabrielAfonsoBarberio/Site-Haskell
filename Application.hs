{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE QuasiQuotes          #-}

module Application where

import Foundation
import Yesod
import Handler.Inventario
import Handler.Cliente
import Handler.Login
import Handler.Relatorio
import Handler.Funcionario

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
    sessao <- lookupSession "_ID"
    toWidget [lucius|
        ul li {
            display: inline;
        }
    |]
    [whamlet|
        <h1> Sistema de Transacoes! 
        <ul>
            <li> <a href=@{PesR}>Cadastro de clientes
            <li> <a href=@{InvR}>Cadastrar itens
            <li> <a href=@{ListPessR}>Listar clientes
            <li> <a href=@{ListInvR}>Inventario
            <li> <a href=@{RelatorioR}>Transacoes
            $maybe sess <- sessao
                <form method=post action=@{LogoutR}>
                    <input type="submit" value="Logout">
            
    |]