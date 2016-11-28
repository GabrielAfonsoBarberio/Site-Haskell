{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Handler.Cliente where

import Foundation
import Yesod
import Database.Persist.Postgresql
import Data.Text

formCliente :: Form Cliente
formCliente = renderDivs $ Cliente
        <$> areq textField "Nome " Nothing
        <*> areq textField "CPF " Nothing
        <*> areq textField "Endereco" Nothing
        <*> areq textField "Cidade " Nothing
        <*> areq textField "Estado" Nothing
        <*> areq textField "Telefone" Nothing

getClienteR :: Handler Html
getClienteR = do
    (widget,enctype) <- generateFormPost formCliente
    defaultLayout $ do
        [whamlet|
            <form action=@{ClienteR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]

postClienteR :: Handler Html
postClienteR = do
        ((result,_),_)<- runFormPost formCliente
        case result of
            FormSuccess clientela -> do
                vid <- runDB $ insert clientela
                defaultLayout [whamlet|
                    <h1> Cliente #{fromSqlKey vid} cadastrado!
                |]
            _ -> redirect HomeR

getListClienteR :: Handler Html
getListClienteR = do
            pess <- runDB $ selectList [] [Asc ClienteNome]
            defaultLayout $ do
                [whamlet|
                     <table>
                         <tr>
                             <td> id
                             <td> nome
                             <td> cpf
                             <td> endereco
                             <td> cidade
                             <td> estado
                             <td> telefone
                         $forall Entity pid pess <- pess
                             <tr>
                                 <td> #{fromSqlKey pid}
                                 <td> #{clienteNome pess}
                                 <td> #{clienteCpf pess}
                                 <td> #{clienteEndereco pess}
                                 <td> #{clienteCidade pess}
                                 <td> #{clienteEstado pess}
                                 <td> #{clienteTelefone pess}
                         
                |]