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


formPess :: Form Pessoa
formPess = renderDivs $ Pessoa
    <$> areq textField "Nome" Nothing
    <*> areq textField "CPF" Nothing
    <*> areq textField "Endereco" Nothing
    <*> areq textField "Cidade" Nothing
    <*> areq textField "Estado" Nothing
    <*> areq textField "Telefone" Nothing
    
getPessR :: Handler Html
getPessR = do
           (widget, enctype) <- generateFormPost formPess
           defaultLayout [whamlet|
             <form method=post action=@{PessR} enctype=#{enctype}>
                 ^{widget}
                 <input type="submit" value="Cadastrar">
           |]

postPessR :: Handler Html
postPessR = do
            ((result, _), _) <- runFormPost formPess
            pessoaid <- fromMaybe  (lookupPostParams "F22") :: Handler [InventarioId]
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
            pessoa <- runDB $ selectList [] [Asc PessoaNome]
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
                         $forall Entity pid pess <- pess
                             <tr>
                                 <td> #{fromSqlKey pid}
                                 <td> #{pessoaNome pess}
                                 <td> #{pessoaCPF pess}
                                 <td> #{pessoaEndereco pess}
                                 <td> #{pessoaCidade pess}
                                 <td> #{pessoaEstado pess}
                                 <td> #{pessoaTelefone pess}
                         
                |]