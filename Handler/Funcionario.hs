{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
             
module Handler.Funcionario where

import Foundation
import Yesod
import Database.Persist.Postgresql
import Data.Text


formFunc :: Form Funcionario
formFunc = renderDivs $ Funcionario
    <$> areq textField "Nome" Nothing
    <*> areq textField "Posicao" Nothing
    <*> areq textField "Email" Nothing
    <*> areq textField "Password" Nothing
