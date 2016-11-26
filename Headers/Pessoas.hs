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
                FormSuccess prof -> do
                    pid <- runDB $ insert prof
                    sequence $ Prelude.map (\x -> runDB $ insert (CursoProfessor pid x)) cursosid 
                    defaultLayout [whamlet|
                        Professor(a) cadastrado(a) com sucesso #{fromSqlKey pid}!
                    |]
                _ -> redirect HomeR

getListProfR :: Handler Html
getListProfR = do
            profs <- runDB $ selectList [] [Asc ProfessorNome]
            defaultLayout $ do
                [whamlet|
                     <table>
                         <tr>
                             <td> id
                             <td> nome
                             <td> rg
                             <td> salario
                         $forall Entity pid prof <- profs
                             <tr>
                                 <td> #{fromSqlKey pid}
                                 <td> #{professorNome prof}
                                 <td> #{professorRg prof}
                                 <td> #{professorSalario prof}
                         
                |]