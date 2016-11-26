import Foundation
import Yesod
import Data.Text
import Database.Persist.Postgresql
-- areq obrigatorio
-- aopt opcional e precisa de maybe
-- # interpolador de comandos haskell
-- @ interpolador de rotas
-- ^ 
-- $ comandos lógicos de front-end
-- _ inter
--https://ide.c9.io/romefeller/hask4
--https://ide.c9.io/romefeller/hask2
formProf :: Form Pessoas
formProf = renderDivs $ Pessoas
    <$> areq textField "Nome" Nothing
    <*> areq textField "CPF" Nothing
    <*> areq textField "Salario" Nothing

formProfCurso :: Form [CursoId]
formProfCurso = renderDivs $ areq (multiSelectField cursosLista) FieldSettings{fsId=Just "hident6",
                           fsLabel="Curso ",
                           fsTooltip= Nothing,
                           fsName = (Just "F22"),
                           fsAttrs=[]} Nothing
            where
                cursosLista = do
                    cursos <- runDB $ selectList [] [Asc CursoNome]
                    optionsPairs $ Prelude.map (\cur -> (cursoNome $ entityVal cur, entityKey cur)) cursos
    
getProfR :: Handler Html
getProfR = do
           (widget, enctype) <- generateFormPost formProf
           (widget2, _) <- generateFormPost formProfCurso
           defaultLayout [whamlet|
             <form method=post action=@{ProfR} enctype=#{enctype}>
             
                 ^{widget}
                 ^{widget2}
                 <input type="submit" value="Cadastrar">
           |]

postProfR :: Handler Html
postProfR = do
            ((result, _), _) <- runFormPost formProf
            cursoid <- fromMaybe  (lookupPostParams "F22") :: Handler [CursoId]
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