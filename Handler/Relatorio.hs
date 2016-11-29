{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Handler.Relatorio where

import Foundation
import Yesod
import Handler.Cliente
import Handler.Inventario
import Handler.Admin
import Database.Persist.Postgresql
import Data.Text
import Data.Monoid

formRelatorio :: Form Relatorio
formRelatorio = renderDivs $ Relatorio 
                    <$> areq (selectField clie) "Cliente" Nothing
                    <*> areq (selectField ite) "Itens" Nothing
                    <*> areq (selectField funci) "Funcionario" Nothing
                    <*> areq textField "Concluido" Nothing
                    
clie = do
       entidades <- runDB $ selectList [] [Asc ClienteNome] 
       optionsPairs $ fmap (\ent -> (clienteNome $ entityVal ent, entityKey ent)) entidades
ite = do
       entidades <- runDB $ selectList [] [Asc InventarioNome] 
       optionsPairs $ fmap (\ent -> (inventarioNome $ entityVal ent, entityKey ent)) entidades
funci = do
       entidades <- runDB $ selectList [] [Asc FuncionarioNome] 
       optionsPairs $ fmap (\ent -> (funcionarioNome $ entityVal ent, entityKey ent)) entidades


getRelatorioR :: Handler Html
getRelatorioR = do
    (widget,enctype) <- generateFormPost formRelatorio
    defaultLayout $ do
        [whamlet|
            <form action=@{RelatorioR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Realizar emprestimo">
        |]
        
postRelatorioR :: Handler Html
postRelatorioR = do
        ((result,_),_) <- runFormPost formRelatorio
        case result of
            FormSuccess relatorio -> do
                        pid <- runDB $ insert relatorio
                        defaultLayout [whamlet| <h1> Emprestimo #{fromSqlKey pid} criado com sucesso! |]
            
      
getListRelatorioR :: Handler Html
getListRelatorioR = undefined
            