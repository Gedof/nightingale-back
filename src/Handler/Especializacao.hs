{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Especializacao where

import Import
import Data.Time
import Handler.Validation
import Handler.JSONTypes





--POST ESPEC

postEspecR :: Handler TypedContent
postEspecR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    bearer <- lookupBearerAuth
    especsjson <- requireJsonBody :: Handler EspecsJSON
    especs <- liftIO $ cleanEspecs $ especsEspecializacoes especsjson
    especids <- forM especs insEspec
    sendStatusJSON created201 (object ["resp" .= especids])
    where
    insEspec e = runDB $ insert e :: Handler EspecializacaoId
    
cleanEspecs :: [EspecJSON] -> IO [Especializacao]
cleanEspecs es = do
    now <- getCurrentTime
    especsf <- return $ map cleanEspec es
    return $ map (\f -> f now) especsf
    where
    cleanEspec e = Especializacao (especNome e)
    
--GET ESPEC

optionsSingleEspecializacaoR :: EspecializacaoId -> Handler TypedContent
optionsSingleEspecializacaoR _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])

getSingleEspecializacaoR :: EspecializacaoId -> Handler TypedContent
getSingleEspecializacaoR especid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    espec <- runDB $ get404 especid
    especjson <- return $ createEspecJSON especid espec
    sendStatusJSON ok200 (object ["resp" .= especjson])
    
createEspecJSON :: EspecializacaoId -> Especializacao -> EspecJSON
createEspecJSON especid espec =
    EspecJSON especid (especializacaoNome espec) Nothing


optionsListEspecR :: Handler TypedContent
optionsListEspecR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])

getListEspecR :: Handler TypedContent
getListEspecR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    bearer <- lookupBearerAuth
    especs <- runDB $ selectList [] [Asc EspecializacaoId]
    especsjson <- return $ createEspecs especs
    sendStatusJSON ok200 (object ["resp" .= especsjson])
    
createEspecs :: [Entity Especializacao] -> [EspecJSON]
createEspecs es = map createEspec es
    where
    createEspec e = EspecJSON (entityKey e) (especializacaoNome $ entityVal e) Nothing
    
    
