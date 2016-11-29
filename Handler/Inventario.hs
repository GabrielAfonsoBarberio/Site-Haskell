{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
             
module Handler.Inventario where

import Foundation
import Yesod
import Database.Persist.Postgresql
import Data.Text

formInv :: Form Inventario
formInv = renderDivs $ Inventario
    <$> areq textField "Nome" Nothing
    <*> areq textField "Tipo" Nothing
    <*> areq textField "Disponivel" Nothing

getInvR :: Handler Html
getInvR = do
           (widget, enctype) <- generateFormPost formInv
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

getListInvR :: Handler Html
getListInvR = do
            item <- runDB $ selectList [] [Asc InventarioNome]
            defaultLayout $ do
                [whamlet|
                     <table>
                         <tr>
                             <td> id
                             <td> nome
                             <td> tipo
                             <td> disponibilidade
                         $forall Entity pid item <- item
                             <tr>
                                 <td> #{fromSqlKey pid}
                                 <td> #{inventarioNome item}
                                 <td> #{inventarioTipo item}
                                 <td> #{inventarioDisponibilidade item}
                |]