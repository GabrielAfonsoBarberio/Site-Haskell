{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE QuasiQuotes          #-}

module Application where

import Foundation
import Yesod
import Yesod.Static
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
                display: inline-block;
                margin-right: 2%;
            }
            a {
                color: blue;
            }
            h1{
                
            }
            ul{
                border: solid 1px;
                background: mintcream;
            }
            div{
                margin-left: 14%;
                border: solid 1px;
                width: 64%;
                padding: 4%;
                text-align: center;
                background: white;
                border-radius: 60%;
                box-shadow: 0px 0px 1px 2px darkblue;
            }
        |]
        [whamlet|
        <div>
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