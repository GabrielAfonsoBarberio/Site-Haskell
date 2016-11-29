{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handler.Login where

import Foundation
import Yesod
import Database.Persist.Postgresql
import Data.Text

formLogin :: Form (Text, Text)
formLogin = renderDivs $ (,) <$>
             areq emailField "E-mail" Nothing <*>
             areq passwordField "Password" Nothing

getLoginR :: Handler Html
getLoginR = do
    (widget,enctype) <- generateFormPost formLogin
    defaultLayout $ do
        [whamlet|
            <form action=@{LoginR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Logar">
        |]

postLoginR :: Handler Html
postLoginR = do
            ((result, _), _) <- runFormPost formLogin
            case result of
                FormSuccess (email, password) -> do
                   cara <- runDB $ selectFirst [FuncionarioEmail ==. email,
                                                FuncionarioSenha ==. password] []
                   case cara of
                       Just (Entity pid funcionarioid "admin") -> do
                            setSession "_ADMIN" "admin"
                            redirect AdminR
                       Just (Entity pid funcionarioid) -> do
                           setSession "_ID" (pack $ show $ fromSqlKey pid)
                           redirect (FuncionarioR pid)
                       Nothing -> redirect LoginR
                _ -> redirect HomeR


getPerfilR :: Handler Html
getPerfilR = do
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
    
postLogoutR:: Handler Html
postLogoutR = do
    deleteSession "_ID"
    redirect HomeR