{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Home where

import Foundation
import Yesod
import Data.Text
import Control.Applicative

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|
                <h1> <a href=@{LoginR}>Por favor, faca o login.
                |]