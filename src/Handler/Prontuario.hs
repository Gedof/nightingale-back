{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Prontuario where

import Import
import Data.Time
import Handler.Validation
import Handler.JSONTypes
import Handler.Login
import Database.Persist.Sql


--POST PRONT
optionsProntuarioR :: Handler TypedContent
optionsProntuarioR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])


-- postProntuarioR :: Handler TypedContent
-- postProntuarioR = do
--     addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
--     addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
--     mbearer <- lookupBearerAuth
--     case mbearer of
--         Just bearer -> do
--             if (jwtData bearer) then do
--                 case (jwtCargo bearer) of
--                     Just 3 -> do
--                         case (jwtId bearer) of
--                             Just bearerid -> do
--                                 prontjson <- requireJsonBody :: Handler ProntReqJSON
--                                 --prontjson <- return $ prontcadEntradaProntuario prontcadjson
--                                 isValid <- validatePront prontjson
--                                 if (not isValid) then
--                                     sendStatusJSON badRequest400 (object ["resp" .= invalido])
--                                 else do
--                                     --medico <- runDB $ getBy404 $ UniqueUserId (prontreqMedicoid prontjson)
--                                     prontuario <- liftIO $ cleanPront prontjson
--                                     prontid <- runDB $ insert prontuario
--                                     sendStatusJSON created201 (object ["resp" .= prontid])
--                             Nothing -> sendStatusJSON unauthorized401 (object [])
--                     Nothing -> sendStatusJSON forbidden403 (object [])
--             else sendStatusJSON unauthorized401 (object [])
--         Nothing -> sendStatusJSON unauthorized401 (object [])
--     where
--             invalido = "Invalido" :: Text

postProntuarioR :: Handler TypedContent
postProntuarioR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    mbearer <- lookupBearerAuth
    prontjson <- requireJsonBody :: Handler ProntReqJSON
    --prontjson <- return $ prontcadEntradaProntuario prontcadjson
    mjwtInfo <- jwtAll mbearer
    case mjwtInfo of
        Just jwtInfo -> do
            case (jwjCargo jwtInfo) of
                x | elem x [3] -> do
                    isValid <- validatePront prontjson
                    emedico <- runDB $ getBy404 $ UniqueUserId $ jwjId jwtInfo
                    medid <- return $ entityKey emedico
                    if (not isValid) then
                        sendStatusJSON badRequest400 (object ["resp" .= invalido])
                    else do
                        --medico <- runDB $ getBy404 $ UniqueUserId (prontreqMedicoid prontjson)
                        prontuario <- liftIO $ cleanPront medid prontjson
                        prontid <- runDB $ insert prontuario
                        sendStatusJSON created201 (object ["resp" .= prontid])
                _ -> sendStatusJSON forbidden403 (object ["resp" .= (1::Int)])
        Nothing -> sendStatusJSON unauthorized401 (object ["resp" .= (1::Int)])
    where
        invalido = "Invalido" :: Text
        
validatePront :: ProntReqJSON -> Handler Bool
validatePront prontjson = do
    --validateMedid <- runDB $ selectList [MedicoUserid ==. (prontreqMedicoid prontjson)] [Asc MedicoId]
    pacid <- fmap tobool $ runDB $ get $ prontreqPacienteid prontjson
    --medid <- return $ (not . null) validateMedid
    --medid <- fmap tobool $ runDB $ get $ prontreqMedicoid prontjson
    --espid <- fmap tobool $ runDB $ get $ prontreqEspecid prontjson
    return $ pacid --espid --medid
    where
    tobool y = case y of
        Nothing -> False
        Just _ -> True

        
cleanPront :: MedicoId -> ProntReqJSON -> IO EntradaProntuario
cleanPront medid prontuario = do
    now <- getCurrentTime
    return $ EntradaProntuario {
        entradaProntuarioPacienteid     = prontreqPacienteid prontuario,
        --entradaProntuarioMedicoid       = prontreqMedicoid prontuario,
        --entradaProntuarioMedicoid       = toSqlKey $ 1,
        entradaProntuarioMedicoid       = medid,
        --entradaProntuarioEspecid        = prontreqEspecid prontuario,
        entradaProntuarioEspecid        = Nothing,
        entradaProntuarioConteudo       = prontreqConteudo prontuario,
        entradaProntuarioTimestamp      = now
    }

    
--GET PRONT

optionsSingleProntuarioR :: EntradaProntuarioId -> Handler TypedContent
optionsSingleProntuarioR  _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])

getSingleProntuarioR :: EntradaProntuarioId -> Handler TypedContent
getSingleProntuarioR prontuarioid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    mbearer <- lookupBearerAuth
    mjwtInfo <- jwtAll mbearer
    case mjwtInfo of
        Just jwtInfo -> do
            case (jwjCargo jwtInfo) of
                x | elem x [3] -> do
                    prontuario <- runDB $ get404 prontuarioid
                    --medico <- runDB $ get404 (entradaProntuarioMedicoid prontuario)
                    prontjson <- return $ createPront prontuarioid prontuario
                    sendStatusJSON ok200 (object ["resp" .= prontjson])
                _ -> sendStatusJSON forbidden403 (object ["resp" .= (1::Int)])
        Nothing -> sendStatusJSON unauthorized401 (object ["resp" .= (1::Int)])
    
    
createPront :: EntradaProntuarioId -> EntradaProntuario -> ProntResJSON
createPront prontuarioid prontuario = 
    ProntResJSON {
        prontresId          = prontuarioid,
        prontresPacienteid  = entradaProntuarioPacienteid prontuario,
        prontresMedicoid    = Just $ entradaProntuarioMedicoid prontuario,
        prontresEspecid     = entradaProntuarioEspecid prontuario,
        prontresConteudo    = entradaProntuarioConteudo prontuario,
        prontresTimestamp   = utcToZonedTime utcTime $ entradaProntuarioTimestamp prontuario
    }
    
optionsListProntuarioR :: Handler TypedContent
optionsListProntuarioR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])
    
getListProntuarioR :: Handler TypedContent
getListProntuarioR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    mbearer <- lookupBearerAuth
    mjwtInfo <- jwtAll mbearer
    case mjwtInfo of
        Just jwtInfo -> do
            case (jwjCargo jwtInfo) of
                x | elem x [3] -> do
                    eprontuarios <- runDB $ selectList [] [Asc EntradaProntuarioId]
                    prontjsons <- forM eprontuarios createPronts
                    sendStatusJSON created201 (object ["resp" .= prontjsons])
                _ -> sendStatusJSON forbidden403 (object ["resp" .= (1::Int)])
        Nothing -> sendStatusJSON unauthorized401 (object ["resp" .= (1::Int)])
    
    
createPronts :: Entity EntradaProntuario -> Handler ProntResJSON
createPronts eprontuario = do 
    --medico <- runDB $ get404 (entradaProntuarioMedicoid prontuario)
    return $ createPront prontuarioid prontuario
    where
    prontuarioid = entityKey eprontuario
    prontuario = entityVal eprontuario

optionsPacProntuarioR :: PacienteId -> Handler TypedContent
optionsPacProntuarioR  _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])
    
getPacProntuarioR :: PacienteId -> Handler TypedContent
getPacProntuarioR pacienteid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    mbearer <- lookupBearerAuth
    mjwtInfo <- jwtAll mbearer
    case mjwtInfo of
        Just jwtInfo -> do
            case (jwjCargo jwtInfo) of
                x | elem x [3] -> do
                    eprontuarios <- runDB $ selectList [EntradaProntuarioPacienteid ==. pacienteid] [Asc EntradaProntuarioId]
                    prontjsons <- forM eprontuarios createPronts
                    sendStatusJSON created201 (object ["resp" .= prontjsons])
                _ -> sendStatusJSON forbidden403 (object ["resp" .= (1::Int)])
        Nothing -> sendStatusJSON unauthorized401 (object ["resp" .= (1::Int)])

    
--apagar

optionsApagarProntuarioR :: EntradaProntuarioId -> Handler TypedContent
optionsApagarProntuarioR  _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])

deleteApagarProntuarioR :: EntradaProntuarioId -> Handler TypedContent
deleteApagarProntuarioR prontuarioid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    bearer <- lookupBearerAuth
    _ <- runDB $ get404 prontuarioid
    runDB $ delete prontuarioid
    sendStatusJSON noContent204 (object [])
