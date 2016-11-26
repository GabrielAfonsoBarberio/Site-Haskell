import Foundation
import Yesod
import Data.Text
import Data.Bool
import Database.Persist.Postgresql

formPess :: Form Pessoas
formPess = renderDivs $ Pessoas
    <$> areq textField "Nome" Nothing
    <*> areq textField "CPF" Nothing
    <*> areq textField "Endereco" Nothing
    <*> areq textField "Cidade" Nothing
    <*> areq textField "Estado" Nothing
    <*> areq textField "Telefone" Nothing
    

formPessObj :: Form [ObjetosId]
formPessObj = renderDivs $ areq (multiSelectField cursosLista) FieldSettings{fsId=Just "hident6",
                           fsLabel="Objeto ",
                           fsTooltip= Nothing,
                           fsName = (Just "F22"),
                           fsAttrs=[]} Nothing
            where
                objLista = do
                    objetos <- runDB $ selectList [] [Asc ObjetosNome]
                    optionsPairs $ Prelude.map (\cur -> (objetosNome $ entityVal cur, entityKey cur)) objetos
    
getPessR :: Handler Html
getPessR = do
           (widget, enctype) <- generateFormPost formPess
           (widget2, _) <- generateFormPost formPessObj
           defaultLayout [whamlet|
             <form method=post action=@{PessR} enctype=#{enctype}>
             
                 ^{widget}
                 ^{widget2}
                 <input type="submit" value="Cadastrar">
           |]

postPessR :: Handler Html
postPessR = do
            ((result, _), _) <- runFormPost formPess
            cursoid <- fromMaybe  (lookupPostParams "F22") :: Handler [ObjetosId]
            case result of
                FormSuccess pess -> do
                    pid <- runDB $ insert pess
                    sequence $ Prelude.map (\x -> runDB $ insert (Relatorio pid x)) objetosid 
                    defaultLayout [whamlet|
                        Pessoa(a) cadastrado(a) com sucesso #{fromSqlKey pid}!
                    |]
                _ -> redirect HomeR

getListPessR :: Handler Html
getListPessR = do
            pesoas <- runDB $ selectList [] [Asc PessoaNome]
            defaultLayout $ do
                [whamlet|
                     <table>
                         <tr>
                             <td> id
                             <td> nome
                             <td> cpf
                             <td> endereco
                             <td> cidade
                             <td> estado
                             <td> telefone
                         $forall Entity pid prof <- profs
                             <tr>
                                 <td> #{fromSqlKey pid}
                                 <td> #{pessoasNome pess}
                                 <td> #{pessoasCPF pess}
                                 <td> #{pessoasEndereco pess}
                                 <td> #{pessoasCidade pess}
                                 <td> #{pessoasEstado pess}
                                 <td> #{pessoasTelefone pess}
                         
                |]