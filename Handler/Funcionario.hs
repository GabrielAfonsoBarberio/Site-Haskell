{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
             
module Handler.Funcionario where

import Foundation
import Yesod
import Handler.Relatorio
import Database.Persist.Postgresql
import Data.Text
    
getPerfilR :: Handler Html
getPerfilR = do
    toWidget [lucius|
        ul li {
            display: inline;
        }
    |]
    [whamlet|
        <h1> Bem-vindo, #{funcionarioNome funcionario}! 
        <ul>
            <li> <a href=@{ClienteR}>Cadastro de clientes
            <li> <a href=@{InvR}>Cadastrar itens
            <li> <a href=@{ListClienteR}>Listar clientes
            <li> <a href=@{ListInvR}>Inventario
            <li> <a href=@{RelatorioR}>Transacoes
            $maybe sess <- sessao
                <form method=post action=@{LogoutR}>
                    <input type="submit" value="Logout">
            
    |]