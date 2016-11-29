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

getFuncionarioR :: Handler Html
getFuncionarioR = do
    userId <- lookupSession "_ID"
    case userId of
       Just str -> do
           funcionario <- runDB $ get404 (read (unpack str))
           defaultLayout [whamlet|
               <h1> Bem-vindo, #{funcionarioNome funcionario}. 
           |]
       Nothing -> defaultLayout [whamlet|
        <h1> Favor logar. 
    |]

getAdminR :: Handler Html
getAdminR = do
    (widget,enctype) <- generateFormPost formFuncionario
    defaultLayout $ do
        [whamlet|
            <form action=@{AdminR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Cadastrar funcionario">
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
