{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
             
module Handler.Admin where

import Foundation
import Yesod
import Database.Persist.Postgresql
import Data.Text

formFuncionario :: Form Funcionario
formFuncionario = renderDivs $ Funcionario
    <$> areq textField "Nome" Nothing
    <*> areq textField "Email" Nothing
    <*> areq textField "Password" Nothing

getAdminR :: Handler Html
getAdminR = do
    (widget,enctype) <- generateFormPost formFuncionario
    defaultLayout $ do
        [whamlet|
            <form action=@{AdminR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]

postAdminR :: Handler Html
postAdminR = do
        ((result,_),_)<- runFormPost formFuncionario
        case result of
            FormSuccess empregado -> do
                vid <- runDB $ insert empregado
                defaultLayout [whamlet|
                    <h1>Funcionario #{funcionarioNome empregado} cadastrado!
                |]
            _ -> redirect AdminR
    
getListAdminR :: Handler Html
getListAdminR = do
            func <- runDB $ selectList [] [Asc FuncionarioNome]
            defaultLayout $ do
                [whamlet|
                     <table>
                         <tr>
                             <td> id
                             <td> nome
                             <td> email
                             <td> senha
                         $forall Entity pid func <- func
                             <tr>
                                 <td> #{fromSqlKey pid}
                                 <td> #{funcionarioNome func}
                                 <td> #{funcionarioEmail func}
                                 <td> #{funcionarioSenha func}
                         
                |]
