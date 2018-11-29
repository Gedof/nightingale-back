{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Consulta where

import Import
import Data.Time
import Handler.Validation
import Handler.JSONTypes
import Handler.Login


--POST AGEND

optionsConsultaR :: Handler TypedContent
optionsConsultaR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])

postConsultaR :: Handler TypedContent
postConsultaR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    mbearer <- lookupBearerAuth
    mjwtInfo <- liftIO $ jwtAll mbearer
    case mjwtInfo of
        Just jwtInfo -> do
            case (jwjCargo jwtInfo) of
                x | elem x [1,2,3] -> do
                    consjson <- requireJsonBody :: Handler ConsReqJSON
                    --consjson <- return $ conscadConsulta conscadjson
                    isValid <- validateCons consjson
                    if (not isValid) then
                        sendStatusJSON badRequest400 (object ["resp" .= invalido])
                    else do
                        --medico <- runDB $ getBy404 $ UniqueUserId (consreqMedicoid consjson)
                        consulta <- liftIO $ cleanCons consjson
                        consultaid <- runDB $ insert consulta
                        sendStatusJSON created201 (object ["resp" .= consultaid])
                _ -> sendStatusJSON forbidden403 (object ["resp" .= (1::Int)])
        Nothing -> sendStatusJSON unauthorized401 (object ["resp" .= (1::Int)])
    where
    invalido = "Invalido" :: Text

validateCons :: ConsReqJSON -> Handler Bool
validateCons consjson = do 
    --validateMedid <- runDB $ selectList [MedicoId ==. (consreqMedicoid consjson)] [Asc MedicoId]
    pacid <- fmap tobool $ runDB $ get $ consreqPacienteid consjson
    medid <- fmap tobool $ runDB $ get $ consreqMedicoid consjson
    --medid <- return $ (not . null) validateMedid
    espid <- fmap tobool $ runDB $ get $ consreqEspecid consjson
    return $ pacid && medid && espid
    where
    tobool y = case y of
        Nothing -> False
        Just _ -> True
        
cleanCons :: ConsReqJSON -> IO Consulta
cleanCons consulta = do
    now <- getCurrentTime
    return $ Consulta {
        consultaPacienteid      = consreqPacienteid consulta,
        consultaMedicoid        = consreqMedicoid consulta,
        consultaEspecid         = consreqEspecid consulta,
        consultaInicio          = zonedTimeToUTC $ consreqInicio consulta,
        consultaTermino         = zonedTimeToUTC $ consreqTermino consulta,
        consultaObservacoes     = consreqObservacoes consulta,
        consultaInsertedTimestamp       = now,
        consultaLastUpdatedTimestamp    = now
    }

    
--GET AGEND
optionsSingleConsultaR :: ConsultaId -> Handler TypedContent
optionsSingleConsultaR _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])


getSingleConsultaR :: ConsultaId -> Handler TypedContent
getSingleConsultaR consultaid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    mbearer <- lookupBearerAuth
    mjwtInfo <- liftIO $ jwtAll mbearer
    case mjwtInfo of
        Just jwtInfo -> do
            case (jwjCargo jwtInfo) of
                x | elem x [1,2,3] -> do
                    consulta <- runDB $ get404 consultaid
                    --medico <- runDB $ get404 (consultaMedicoid consulta)
                    consjson <- return $ createCons consultaid consulta
                    sendStatusJSON created201 (object ["resp" .= consjson])
                _ -> sendStatusJSON forbidden403 (object ["resp" .= (1::Int)])
        Nothing -> sendStatusJSON unauthorized401 (object ["resp" .= (1::Int)])
    
    
createCons :: ConsultaId -> Consulta -> ConsResJSON
createCons consultaid consulta = 
    ConsResJSON {
        consresId           = consultaid,
        consresPacienteid   = consultaPacienteid consulta,
        consresMedicoid     = consultaMedicoid consulta,
        consresEspecid      = consultaEspecid consulta,
        consresInicio       = utcToZonedTime utcTime $ consultaInicio consulta,
        consresTermino      = utcToZonedTime utcTime $ consultaTermino consulta,
        consresObservacoes  = consultaObservacoes consulta,
        consresInsertedTimestamp    = utcToZonedTime utcTime $ consultaInsertedTimestamp consulta,
        consresLastUpdatedTimestamp = utcToZonedTime utcTime $ consultaLastUpdatedTimestamp consulta
    }
    
optionsListConsultaR :: Handler TypedContent
optionsListConsultaR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])
    
getListConsultaR :: Handler TypedContent
getListConsultaR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    mbearer <- lookupBearerAuth
    mjwtInfo <- liftIO $ jwtAll mbearer
    case mjwtInfo of
        Just jwtInfo -> do
            case (jwjCargo jwtInfo) of
                x | elem x [1,2,3] -> do
                    econsultas <- runDB $ selectList [] [Asc ConsultaId]
                    consjsons <- forM econsultas createConss
                    sendStatusJSON ok200 (object ["resp" .= consjsons])
                _ -> sendStatusJSON forbidden403 (object ["resp" .= (1::Int)])
        Nothing -> sendStatusJSON unauthorized401 (object ["resp" .= (1::Int)])
    
    
    
createConss :: Entity Consulta -> Handler ConsResJSON
createConss econsulta = do 
    --medico <- runDB $ get404 (consultaMedicoid consulta)
    return $ createCons consultaid consulta
    where
    consultaid = entityKey econsulta
    consulta = entityVal econsulta
    

optionsMedConsultaR :: MedicoId -> Handler TypedContent
optionsMedConsultaR _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])    
    
getMedConsultaR :: MedicoId -> Handler TypedContent
getMedConsultaR medicoid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    mbearer <- lookupBearerAuth
    mjwtInfo <- liftIO $ jwtAll mbearer
    case mjwtInfo of
        Just jwtInfo -> do
            case (jwjCargo jwtInfo) of
                x | elem x [1,2,3] -> do
                    econsultas <- runDB $ selectList [ConsultaMedicoid ==. medicoid] [Asc ConsultaId]
                    consjsons <- forM econsultas createConss
                    sendStatusJSON created201 (object ["resp" .= consjsons])
                _ -> sendStatusJSON forbidden403 (object ["resp" .= (1::Int)])
        Nothing -> sendStatusJSON unauthorized401 (object ["resp" .= (1::Int)])
    
--apagar

optionsApagarConsultaR :: ConsultaId -> Handler TypedContent
optionsApagarConsultaR _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "DELETE"
    sendStatusJSON ok200 (object [])   

deleteApagarConsultaR :: ConsultaId -> Handler TypedContent
deleteApagarConsultaR consultaid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "DELETE"
    mbearer <- lookupBearerAuth
    mjwtInfo <- liftIO $ jwtAll mbearer
    case mjwtInfo of
        Just jwtInfo -> do
            case (jwjCargo jwtInfo) of
                x | elem x [1,2,3] -> do
                    _ <- runDB $ get404 consultaid
                    runDB $ delete consultaid
                    sendStatusJSON ok200 (object ["resp" .= (1::Int)])
                _ -> sendStatusJSON forbidden403 (object ["resp" .= (1::Int)])
        Nothing -> sendStatusJSON unauthorized401 (object ["resp" .= (1::Int)])
    
    

--alterar
optionsAlterarConsultaR :: ConsultaId -> Handler TypedContent
optionsAlterarConsultaR _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "PUT"
    sendStatusJSON ok200 (object [])  

putAlterarConsultaR :: ConsultaId -> Handler TypedContent
putAlterarConsultaR consultaid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "PUT"
    mbearer <- lookupBearerAuth
    mjwtInfo <- liftIO $ jwtAll mbearer
    case mjwtInfo of
        Just jwtInfo -> do
            case (jwjCargo jwtInfo) of
                x | elem x [1,2,3] -> do
                    consulta <- runDB $ get404 consultaid
                    consjson <- requireJsonBody :: Handler ConsReqJSON
                    --consjson <- return $ conscadConsulta conscadjson
                    isValid <- validateCons consjson
                    if (not isValid) then
                        sendStatusJSON badRequest400 (object ["resp" .= invalido])
                    else do
                        --medico <- runDB $ getBy404 $ UniqueUserId (consreqMedicoid consjson)
                        cleancons <- liftIO $ cleanAltCons consjson consulta
                        runDB $ replace consultaid cleancons
                        sendStatusJSON ok200 (object ["resp" .= consultaid])
                _ -> sendStatusJSON forbidden403 (object ["resp" .= (1::Int)])
        Nothing -> sendStatusJSON unauthorized401 (object ["resp" .= (1::Int)])
    where
    invalido = "Invalido" :: Text

cleanAltCons :: ConsReqJSON -> Consulta -> IO Consulta
cleanAltCons consjson consulta = do
    now <- getCurrentTime
    return $ Consulta {
        consultaPacienteid      = consreqPacienteid consjson,
        consultaMedicoid        = consreqMedicoid consjson,
        consultaEspecid         = consreqEspecid consjson,
        consultaInicio          = zonedTimeToUTC $ consreqInicio consjson,
        consultaTermino         = zonedTimeToUTC $ consreqTermino consjson,
        consultaObservacoes     = consreqObservacoes consjson,
        consultaInsertedTimestamp       = consultaInsertedTimestamp consulta,
        consultaLastUpdatedTimestamp    = now
    }