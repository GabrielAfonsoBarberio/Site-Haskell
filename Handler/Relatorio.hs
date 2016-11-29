{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Handler.Relatorio where

import Foundation
import Yesod
import Handler.Cliente
import Handler.Funcionario
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
        ((result,_),_)<- runFormPost formRelatorio
        case result of
            FormSuccess relatorio -> do
                userId <- lookupSession "_ID"
                case userId of
                    Nothing -> redirect HomeR
                    Just userStr -> do
                        pid <- (return $ read $ unpack userStr) :: Handler FuncionarioId
                        sequence $ fmap (\vid -> runDB $ insert $ RelatorioId pid vid) relatorio
                        defaultLayout [whamlet| <h1> Emprestimo #{fromSqlKey pid} criado com sucesso! |]
            _ -> redirect HomeR
            
            
      
getListRelatorioR :: Handler Html
getListRelatorioR = do
            relatorio <- runDB $ selectList [] [Asc RelatorioId]
            defaultLayout $ do
                [whamlet|
                     <table>
                         <tr>
                             <td> id
                             <td> clienteNome
                             <td> funcionarioNome
                             <td> inventarioNome
                             <td> inventarioTipo
                         $forall Entity pid item <- relatorio
                             <tr>
                                 <td> #{fromSqlKey pid}
                                 <td> #{clienteNome item}
                                 <td> #{funcionarioNome item}
                                 <td> #{inventarioNome item}
                                 <td> #{inventarioTipo item}
                |]
                
putUpdateRelatorioR :: [RelatorioId] -> Handler ()
putUpdateRelatorioR pid = do
    pers <- requireJsonBody :: Handler Relatorio
    runDB $ get404 pid
    runDB $ update pid [concluido =. "sim"]
    perss <- requireJsonBody :: Handler Inventario
    runDB $ get404 inventarioNome
    rundDB $ update inventarioNome [inventarioDisponibilidade =. "sim"]
    sendResponse (object [pack "resp" .= pack "Transacao completa!"])  