formRelatorio :: Form [VooId]
formRelatorio = renderDivs $ areq (multiSelectField relatorioLista) "Emprestimos" Nothing
              where
                relatorioLista = do
                    relatorio <- runDB $ selectList [] [Asc RelatorioID]
                    optionsPairs $ Prelude.map (\v -> (mconcat ["Relatorio Num: ", RelatorioID $ entityVal v, "  ", PessoaNome $ entityVal v, " - ", pack $ show $ vooPreco $ entityVal v, " - ", pack $ show $ vooEmbarque $ entityVal v], entityKey v)) relatorio


postRelatorioR :: Handler Html
postRelatorioR = do
            ((result, _), _) <- runFormPost formRela
            pessoaid <- fromMaybe  (lookupPostParams "F22") :: Handler [InventarioId]
            case result of
                FormSuccess pess -> do
                    pid <- runDB $ insert pess
                    sequence $ Prelude.map (\x -> runDB $ insert (Pessoa pid x) inventarioid 
                    defaultLayout [whamlet|
                        Pessoa(a) cadastrado(a) com sucesso #{fromSqlKey pid}!
                    |]
                _ -> redirect HomeR
                
getRelatorioR :: Handler Html
getRelatorioR = do
