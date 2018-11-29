{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Paciente where

import Import
import Data.Time
import Handler.Validation
import Handler.JSONTypes
import Handler.Prontuario
import Handler.Consulta
import Handler.Login
import Data.Text as T (pack,unpack,Text)


optionsPacienteR :: Handler TypedContent
optionsPacienteR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])


--POST PACIENTE
postPacienteR :: Handler TypedContent
postPacienteR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    mbearer <- lookupBearerAuth
    mjwtInfo <- jwtAll mbearer
    case mjwtInfo of
        Just jwtInfo -> do
            case (jwjCargo jwtInfo) of
                x | elem x [1,2,3] -> do
                    pacjson <- requireJsonBody :: Handler PacReqJSON
                    --pacjson <- return $ paccadPaciente paccadjson
                    isValid <- return $ validatePacCad pacjson
                    if (not isValid) then
                        sendStatusJSON badRequest400 (object ["resp" .= invalido])
                    else do
                        cleanpac <- liftIO $ cleanPaciente pacjson
                        pacienteid <- runDB $ insert cleanpac
                        sendStatusJSON created201 (object ["resp" .= pacienteid])
                _ -> sendStatusJSON forbidden403 (object ["resp" .= (1::Int)])
        Nothing -> sendStatusJSON unauthorized401 (object ["resp" .= (1::Int)])
    where
    invalido = "Invalido" :: Text
    
    
validatePacCad :: PacReqJSON -> Bool
validatePacCad pacjson = validPac && validEnds && validTel
    where
    validPac = validatePac (pacreqCpf pacjson) (pacreqRg pacjson)
    validEnds = validateEnd (pacreqCep pacjson) (pacreqEstado pacjson)
    validTel = validateTels (pacreqTelefone pacjson) (pacreqCelular pacjson)
    
validateTels :: Maybe Text -> Maybe Text -> Bool
--validateTels Nothing Nothing = False
validateTels _ _ = True

validatePac :: Text -> Text -> Bool
validatePac cpf rg = valCpf && valRg
    where
    valCpf = cpfCheck cpf
    valRg = rgCheck rg

validateEnd :: Maybe Text -> Maybe Text -> Bool
validateEnd mcep mestado = valCep && valEst
    where
    --valPais = paisCheck $ unpack $ pais end
    valCep = case mcep of
        Just "" -> True
        Just cep -> cepCheck cep
        Nothing -> True
    valEst = case mestado of
        Just "" -> True
        Just estado -> estadoCheck estado
        Nothing -> True


cleanPaciente :: PacReqJSON -> IO Paciente
cleanPaciente pac = do
    now <- getCurrentTime
    return $ Paciente {
        pacienteNome        = cleanAlphabet $ pacreqNome pac,
        pacienteCpf         = cleanNumber $ pacreqCpf pac,
        pacienteRg          = cleanRg,
        pacienteNasc        = pacreqNasc pac,
        pacienteTelefone    = fmap cleanNumber $ pacreqTelefone pac,
        pacienteCelular     = fmap cleanNumber $ pacreqCelular pac,
        pacienteEmail       = pacreqEmail pac,
        pacientePais        = Just "BR",
        pacienteCep         = fmap cleanNumber $ pacreqCep pac,
        pacienteEstado      = pacreqEstado pac,
        pacienteCidade      = pacreqCidade pac,
        pacienteBairro      = pacreqBairro pac,
        pacienteLogradouro  = pacreqLogradouro pac,
        pacienteNumero      = pacreqNumero pac,
        pacienteComplemento = pacreqComplemento pac,
        pacienteInsertedTimestamp       = now,
        pacienteLastUpdatedTimestamp    = now
    }
    where
    cleanNumber x = filterNumber x
    cleanRg = rgFormat $ pacreqRg pac
    cleanAlphabet x = filterAlphabet x


--GET PERFIL  

optionsSinglePacienteR :: PacienteId -> Handler TypedContent
optionsSinglePacienteR _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])
    
getSinglePacienteR :: PacienteId -> Handler TypedContent
getSinglePacienteR pacienteid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    mbearer <- lookupBearerAuth
    mjwtInfo <- jwtAll mbearer
    case mjwtInfo of
        Just jwtInfo -> do
            case (jwjCargo jwtInfo) of
                x | elem x [1,2,3] -> do
                    paciente <- runDB $ get404 pacienteid :: Handler Paciente
                    pacgetjson <- return $ createPacGet pacienteid paciente
                    sendStatusJSON ok200 (object ["resp" .= pacgetjson])
                _ -> sendStatusJSON forbidden403 (object ["resp" .= (1::Int)])
        Nothing -> sendStatusJSON unauthorized401 (object ["resp" .= (1::Int)])
    
    
createPacGet :: PacienteId -> Paciente -> PacResJSON
createPacGet pacienteid pac =
    PacResJSON {
        pacresId            = pacienteid,
        pacresNome          = pacienteNome pac,
        pacresCpf           = pacienteCpf pac,
        pacresRg            = pacienteRg pac,
        pacresNasc          = pacienteNasc pac,
        pacresTelefone      = pacienteTelefone pac,
        pacresCelular       = pacienteCelular pac,
        pacresEmail         = pacienteEmail pac,
        pacresCep           = pacienteCep pac,
        pacresEstado        = pacienteEstado pac,
        pacresCidade        = pacienteCidade pac,
        pacresBairro        = pacienteBairro pac,
        pacresLogradouro    = pacienteLogradouro pac,
        pacresNumero        = pacienteNumero pac,
        pacresComplemento   = pacienteComplemento pac,
        pacresInsertedTimestamp     = istamp,
        pacresLastUpdatedTimestamp  = estamp
    }
    where
    istamp = utcToZonedTime utcTime $ pacienteInsertedTimestamp pac
    estamp = utcToZonedTime utcTime $ pacienteLastUpdatedTimestamp pac


-- optionsListPacienteR :: Handler TypedContent
-- optionsListPacienteR = do
--     addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
--     addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
--     sendStatusJSON ok200 (object [])

-- getListPacienteR :: Handler TypedContent
-- getListPacienteR = do
--     addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
--     addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
--     bearer <- lookupBearerAuth
--     epacientes <- runDB $ selectList [] [Asc PacienteId]
--     pacientes <- return $ map createPacGetE epacientes
--     sendStatusJSON ok200 (object ["resp" .= pacientes])

createPacGetE :: Entity Paciente -> PacResJSON
createPacGetE epaciente = createPacGet pacienteid paciente
    where
    paciente = entityVal epaciente
    pacienteid = entityKey epaciente


--Delete

optionsApagarPacienteR :: PacienteId -> Handler TypedContent
optionsApagarPacienteR _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "DELETE"
    sendStatusJSON ok200 (object [])

deleteApagarPacienteR :: PacienteId -> Handler TypedContent
deleteApagarPacienteR pacienteid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "DELETE"
    mbearer <- lookupBearerAuth
    mjwtInfo <- jwtAll mbearer
    case mjwtInfo of
        Just jwtInfo -> do
            case (jwjCargo jwtInfo) of
                x | elem x [1] -> do
                    _ <- runDB $ get404 pacienteid
                    econs <- runDB $ selectList [ConsultaPacienteid ==. pacienteid] [Asc ConsultaId]
                    epronts <- runDB $ selectList [EntradaProntuarioPacienteid ==. pacienteid] [Asc EntradaProntuarioId]
                    _ <- forM econs apagaCons
                    _ <- forM epronts apagaPront
                    runDB $ delete pacienteid
                    sendStatusJSON ok200 (object ["resp" .= (1::Int)])
                _ -> sendStatusJSON forbidden403 (object ["resp" .= (1::Int)])
        Nothing -> sendStatusJSON unauthorized401 (object ["resp" .= (1::Int)])
    where
    apagaCons econ = deleteApagarConsultaR (entityKey econ)
    apagaPront epront = deleteApagarProntuarioR (entityKey epront)

    
--Alterar

optionsAlterarPacienteR :: PacienteId -> Handler TypedContent
optionsAlterarPacienteR _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "PUT"
    sendStatusJSON ok200 (object [])

putAlterarPacienteR :: PacienteId -> Handler TypedContent
putAlterarPacienteR pacienteid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "PUT"
    mbearer <- lookupBearerAuth
    mjwtInfo <- jwtAll mbearer
    case mjwtInfo of
        Just jwtInfo -> do
            case (jwjCargo jwtInfo) of
                x | elem x [1,2,3] -> do
                    paciente <- runDB $ get404 pacienteid
                    pacjson <- requireJsonBody :: Handler PacReqJSON
                    --pacjson <- return $ paccadPaciente paccadjson
                    isValid <- return $ validatePacCad pacjson
                    if (not isValid) then
                        sendStatusJSON badRequest400 (object ["resp" .= invalido])
                    else do
                        cleanpac <- liftIO $ cleanAltPaciente pacjson $ pacienteInsertedTimestamp paciente
                        runDB $ replace pacienteid cleanpac
                        sendStatusJSON ok200 (object ["resp" .= pacienteid])
                _ -> sendStatusJSON forbidden403 (object ["resp" .= (1::Int)])
        Nothing -> sendStatusJSON unauthorized401 (object ["resp" .= (1::Int)])
    where
    invalido = "Invalido" :: Text
    
cleanAltPaciente :: PacReqJSON -> UTCTime -> IO Paciente
cleanAltPaciente pac timestamp = do
    now <- getCurrentTime
    return $ Paciente {
        pacienteNome        = cleanAlphabet $ pacreqNome pac,
        pacienteCpf         = cleanNumber $ pacreqCpf pac,
        pacienteRg          = cleanRg,
        pacienteNasc        = pacreqNasc pac,
        pacienteTelefone    = fmap cleanNumber $ pacreqTelefone pac,
        pacienteCelular     = fmap cleanNumber $ pacreqCelular pac,
        pacienteEmail       = pacreqEmail pac,
        pacientePais        = Just "BR",
        pacienteCep         = fmap cleanNumber $ pacreqCep pac,
        pacienteEstado      = pacreqEstado pac,
        pacienteCidade      = pacreqCidade pac,
        pacienteBairro      = pacreqBairro pac,
        pacienteLogradouro  = pacreqLogradouro pac,
        pacienteNumero      = pacreqNumero pac,
        pacienteComplemento = pacreqComplemento pac,
        pacienteInsertedTimestamp       = timestamp,
        pacienteLastUpdatedTimestamp    = now
    }
    where
    cleanNumber x = filterNumber x
    cleanRg = rgFormat $ pacreqRg pac
    cleanAlphabet x = filterAlphabet x
    

--Busca

optionsListPacienteR :: Handler TypedContent
optionsListPacienteR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])

getListPacienteR :: Handler TypedContent
getListPacienteR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    mbearer <- lookupBearerAuth
    mjwtInfo <- jwtAll mbearer
    case mjwtInfo of
        Just jwtInfo -> do
            case (jwjCargo jwtInfo) of
                x | elem x [1,2,3] -> do
                    mNome <- lookupGetParam $ T.pack "nome"
                    mRg <- lookupGetParam $ T.pack "rg"
                    mCpf <- lookupGetParam $ T.pack "cpf"
                    nomeFilter <- return $ createPacFilterNome mNome
                    rgFilter <- return $ createPacFilterRg mRg
                    cpfFilter <- return $ createPacFilterCpf mCpf
                    epacientes <- runDB $ selectList (concat [nomeFilter, rgFilter, cpfFilter]) [Asc PacienteId]
                    pacientes <- return $ map createPacGetE epacientes
                    sendStatusJSON ok200 (object ["resp" .= pacientes,"params" .= [mNome, mRg, mCpf]])
                _ -> sendStatusJSON forbidden403 (object ["resp" .= (1::Int)])
        Nothing -> sendStatusJSON unauthorized401 (object ["resp" .= (1::Int)])
    


createPacFilterNome :: Maybe Text -> [Filter Paciente]
createPacFilterNome mNome = 
    case mNome of
        Just nome -> [Filter PacienteNome (Left $ concat ["%", nome, "%"]) (BackendSpecificFilter "ILIKE")]
        Nothing -> []
        
createPacFilterRg :: Maybe Text -> [Filter Paciente]
createPacFilterRg mRg = 
    case mRg of
        Just rg -> [Filter PacienteRg (Left $ concat ["%", rg, "%"]) (BackendSpecificFilter "ILIKE")]
        Nothing -> []

createPacFilterCpf :: Maybe Text -> [Filter Paciente]
createPacFilterCpf mCpf = 
    case mCpf of
        Just cpf -> [Filter PacienteCpf (Left $ concat ["%", cpf, "%"]) (BackendSpecificFilter "ILIKE")]
        Nothing -> []
    
-- funcionarios/list/#pagina?cpf=11111111111&nome=Guilherme&cargo=1