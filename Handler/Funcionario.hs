{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
             
module Handler.Funcionario where

import Foundation
import Yesod
import Database.Persist.Postgresql
import Data.Text
    
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