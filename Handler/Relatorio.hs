{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Handler.Relatorio where

import Foundation
import Yesod
import Handler.Cliente
import Handler.Funcionario
import Handler.Inventario
import Handler.AdminR
import Database.Persist.Postgresql
import Data.Text
import Data.Monoid

formRelatorio :: Form Relatorio
formRelatorio = renderDivs $ RelatorioId 
                    <$> areq ClienteId "Cliente" 
                    <*> areq ItemId "Itens"
                    <*> areq FuncionarioId "Funcionario"
              

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
                             <td> cliente
                             <td> responsavel
                             <td> nome item
                             <td> tipo
                         $forall Entity pid item <- item
                             <tr>
                                 <td> #{fromSqlKey pid}
                                 <td> #{clienteNome item}
                                 <td> #{funcionarioNome item}
                                 <td> #{inventarioNome item}
                                 <td> #{inventarioTipo item}
                |]