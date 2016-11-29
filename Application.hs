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
import Handler.Admin
import Database.Persist.Postgresql


------------------
mkYesodDispatch "Sitio" resourcesSitio


getHomeR :: Handler Html
getHomeR = do
    sessao <- lookupSession "_ID"
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
        <h1> Bem-vindo! 
        <ul>
            <li> <a href=@{ClienteR}>Cadastro de clientes
            <li> <a href=@{InvR}>Cadastrar itens
            <li> <a href=@{ListAdminR}>Lista de funcionarios
            <li> <a href=@{ListClienteR}>Listar clientes
            <li> <a href=@{ListInvR}>Inventario
            <li> <a href=@{RelatorioR}>Transacoes
            <li> <a href=@{AdminR}>Administrador
            $maybe sess <- sessao
                <form method=post action=@{LogoutR}>
                    <input type="submit" value="Logout">
            $maybe _ <- sessao
            <h1> Faca o <a href=@{LoginR}>Login
        |]