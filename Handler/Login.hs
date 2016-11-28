{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handler.Login where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Text.Lucius
import Database.Persist.Postgresql

formLogin :: Form (Text, Text)
formLogin = renderDivs $ (,) <$>
             areq emailField "E-mail" Nothing <*>
             areq passwordField "Password" Nothing

getLoginR :: Handler Html
getLoginR = do
    (widget, enctype) <- generateFormPost formLogin
    defaultLayout $ widgetForm LoginR enctype widget "Login"

postLoginR :: Handler Html
postLoginR = do
            ((result, _), _) <- runFormPost formLogin
            case result of
                FormSuccess (email, password) -> do
                   cara <- runDB $ selectFirst [FuncionarioEmail ==. email,
                                                FuncionarioSenha ==. password] []
                   case cara of
                       Just (Entity pid pessoasid) -> do
                           setSession "_ID" (pack $ show $ fromSqlKey pid)
                           redirect (PerfilR pid)
                       Nothing -> redirect LoginR
                _ -> redirect CadastroR

postLogoutR:: Handler Html
postLogoutR = do
    deleteSession "_ID"
    redirect HomeR
    
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Handler.Login where
import Foundation
import Yesod
import Database.Persist.Postgresql
import Data.Text

formLogin :: Form (Text, Text)
formLogin = renderDivs $ (,)
        <$> areq emailField  "Email "     Nothing
        <*> areq passwordField "Password" Nothing   

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
        ((result,_),_)<- runFormPost formLogin
        case result of
            FormSuccess ("admin","admin") -> do
                setSession "_ADMIN" "admin"
                redirect AdminR
            FormSuccess (login,senha) -> do
                usuario <- runDB $ selectFirst [PassageiroEmail ==. login, 
                                                PassageiroSenha ==. senha] []
                case usuario of
                    Just pass -> do
                        setSession "_ID" (pack $ show $ entityKey pass) 
                        redirect ReservaR
                    Nothing -> redirect LoginR
            _ -> redirect HomeR

getAdminR :: Handler Html
getAdminR = defaultLayout [whamlet| <h1> Bem-vindo ADMIN! |]

postLogoutR :: Handler ()
postLogoutR = do
    deleteSession "_ID"
    redirect HomeR