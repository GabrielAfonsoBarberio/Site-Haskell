{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
             
module Handlers.Inventario where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Text.Lucius
import Database.Persist.Postgresql
import Database.Persist

formInv :: Form Inventario
formInv = renderDivs $ Inventario
    <$> areq textField "Nome" Nothing
    <*> areq textField "Tipo" Nothing
    <*> areq bool      "Disponivel" False

getInvR :: Handler Html
getInvR = do
           (widget, enctype) <- generateFormPost formCurso
           defaultLayout [whamlet|
             <form method=post action=@{InvR} enctype=#{enctype}>
                 ^{widget}
                 <input type="submit" value="Cadastrar">
           |]

postInvR :: Handler Html
postInvR = do
            ((result, _), _) <- runFormPost formInv
            case result of
                FormSuccess inv -> do
                    pid <- runDB $ insert inv
                    defaultLayout [whamlet|
                        Item cadastrado com sucesso #{fromSqlKey pid}!
                    |]
                _ -> redirect HomeR

postInvR :: Handler Html
postCadastroR = do
                ((result, _), _) <- runFormPost formPessoa
                case result of
                    FormSuccess pessoa -> do
                       unicoCodigo <- runDB $ getBy $ UniqueEmail (pessoaEmail inventario)
                       case unicoEmail of
                           Just _ -> redirect CadastroR
                           Nothing -> do 
                              pid <- runDB $ insert pessoa 
                              redirect (PessoaR pid)
                    _ -> redirect CadastroR


getListInvR :: Handler Html
getListInvR = undefined