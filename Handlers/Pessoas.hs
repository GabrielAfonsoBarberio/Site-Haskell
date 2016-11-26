{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
             
module Handlers.Pessoas where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Text.Lucius
import Database.Persist.Postgresql
import Database.Persist


formPess :: Form Pessoas
formPess = renderDivs $ Pessoas
    <$> areq textField "Nome" Nothing
    <*> areq textField "CPF" Nothing
    <*> areq textField "Endereco" Nothing
    <*> areq textField "Cidade" Nothing
    <*> areq textField "Estado" Nothing
    <*> areq textField "Telefone" Nothing
    

formPessInv :: Form [InventarioId]
formPessInv = renderDivs $ areq (multiSelectField cursosLista) FieldSettings{fsId=Just "hident6",
                           fsLabel="Inventario ",
                           fsTooltip= Nothing,
                           fsName = (Just "F22"),
                           fsAttrs=[]} Nothing
            where
                invLista = do
                    inventario <- runDB $ selectList [] [Asc InventarioNome]
                    optionsPairs $ Prelude.map (\cur -> (objetosNome $ entityVal cur, entityKey cur)) inventario
    
getPessR :: Handler Html
getPessR = do
           (widget, enctype) <- generateFormPost formPess
           (widget2, _) <- generateFormPost formPessInv
           defaultLayout [whamlet|
             <form method=post action=@{PessR} enctype=#{enctype}>
             
                 ^{widget}
                 ^{widget2}
                 <input type="submit" value="Cadastrar">
           |]

postPessR :: Handler Html
postPessR = do
            ((result, _), _) <- runFormPost formPess
            cursoid <- fromMaybe  (lookupPostParams "F22") :: Handler [InventarioId]
            case result of
                FormSuccess pess -> do
                    pid <- runDB $ insert pess
                    sequence $ Prelude.map (\x -> runDB $ insert (Relatorio pid x)) inventarioid 
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