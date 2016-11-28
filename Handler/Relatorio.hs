{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Handler.Relatorio where

import Foundation
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Data.Monoid

formRelatorio :: Form [RelatorioId]
formRelatorio = renderDivs $ areq (multiSelectField relatorioLista) "Emprestimos" Nothing
              where
                relatorioLista = do
                    relatorio <- runDB $ selectList [] [Asc RelatorioId]
                    optionsPairs $ Prelude.map (\v -> (mconcat ["Relatorio Num: ", relatid $ entityVal v, "  ", pessnome $ entityVal v, " - ", pack $ show $ itemnome $ entityVal v, " - ", pack $ show $ vooEmbarque $ entityVal v], entityKey v)) relatorio


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
                        pid <- (return $ read $ unpack userStr) :: Handler UsuarioId
                        sequence $ fmap (\vid -> runDB $ insert $ RelatorioId pid vid) relatorio
                        defaultLayout [whamlet| <h1> Emprestimo #{fromSqlKey pid} criado com sucesso! |]
            _ -> redirect HomeR         