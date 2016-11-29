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
import Handler.Admin

-- AQUI MORAM OS HANDLERS
-- import Add
-- PARA CADA NOVO GRUPO DE HANDLERS, CRIAR UM AQUIVO
-- DE HANDLER NOVO E IMPORTAR AQUI


------------------
mkYesodDispatch "App" resourcesApp


getHomeR :: Handler Html
getHomeR = do
    sess <- lookupSession "_ADMIN"
    defaultLayout $ do
        toWidget [lucius|
            ul li {
                display: inline;
            }
            a {
                color: blue;
            }
        |]
        [whamlet|
        <h1> Bem-vindo, #{funcionarioNome funcionario}! 
        <ul>
            <li> <a href=@{ClienteR}>Cadastro de clientes
            <li> <a href=@{InvR}>Cadastrar itens
            <li> <a href=@{ListAdminR}>Lista de funcionarios
            <li> <a href=@{ListClienteR}>Listar clientes
            <li> <a href=@{ListInvR}>Inventario
            <li> <a href=@{RelatorioR}>Transacoes
            <li> <a href=@{UpdateRelatorioR}>Atualizar transacao
            $maybe sess <- sessao
                <form method=post action=@{LogoutR}>
                    <input type="submit" value="Logout">
            $nothing
            <h1> Faca o <a href=@{LoginR}>Login
        |]