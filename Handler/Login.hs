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
                <input type="submit" class=btn value="Logar">
        |]

postLoginR :: Handler Html
postLoginR = do
            ((result, _), _) <- runFormPost formLogin
            case result of
                FormSuccess ("admin", "admin") -> do
                    setSession "_ADMIN" "admin"
                    redirect AdminR
                FormSuccess (email, password) -> do
                   cara <- runDB $ selectFirst [FuncionarioEmail ==. email,
                                                FuncionarioSenha ==. password] []
                   case cara of
                       Just (Entity pid funcionario) -> do
                           setSession "_ID" (pack $ show $ fromSqlKey pid)
                           redirect HomeR
                       Nothing -> redirect LoginR
                _ -> redirect HomeR
    
postLogoutR:: Handler Html
postLogoutR = do
    deleteSession "_ID"
    redirect HomeR