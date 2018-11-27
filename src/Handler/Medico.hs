{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Medico where

import Import
import Data.Time
import Handler.Validation
import Handler.JSONTypes
import Data.Text as T (pack,unpack,Text)
import Text.Read (readMaybe)
import Database.Persist.Sql
--import Data.Hash.MD5

optionsMedicoR :: Handler TypedContent
optionsMedicoR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])


--POST MEDICO

postMedicoR :: Handler TypedContent
postMedicoR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    bearer <- lookupBearerAuth
    medjson <- requireJsonBody :: Handler MedReqJSON
    --medjson <- return $ medcadMedico medcadjson
    isValid <- validateMedCad medjson
    if (not isValid) then
        sendStatusJSON badRequest400 (object ["resp" .= invalido])
    else do
        cleanusu <- liftIO $ cleanUsu medjson
        usuid <- runDB $ insert cleanusu
        cleanmed <- return $ cleanMed usuid $ medreqCrm medjson
        medid <- runDB $ insert cleanmed
        cleanespecmeds <- liftIO $ cleanEspecMeds medid $ medreqEspecializacoes medjson
        especmedics <- forM cleanespecmeds insEspecMed
        sendStatusJSON created201 (object ["resp" .= (object ["usuarioid" .= usuid,"medicoid" .= medid]),"bearer" .= bearer])
    where
    invalido = "Invalido" :: Text
    insEspecMed e = runDB $ insert e :: Handler EspecMedicoId

validateMedCad :: MedReqJSON -> Handler Bool
validateMedCad medjson = do 
    validEspec <- validateEspecs (medreqEspecializacoes medjson)
    validMed <- return $ validateMed (medreqCpf medjson) (medreqRg medjson)
    validEnds <- return $ validateEnd (medreqCep medjson) (medreqEstado medjson)
    validTel <- return $ validateTels (medreqTelefone medjson) (medreqCelular medjson)
    return $ (validMed && validEnds && validTel && validEspec)
    

validateEspecs :: [EspecializacaoId] -> Handler Bool
validateEspecs especids = do
    validEspecs <- forM especids validateEspec
    return $ not $ elem False $ validEspecs
    
validateEspec :: EspecializacaoId -> Handler Bool
validateEspec especid = fmap tobool $ runDB $ get especid
    where
    tobool x = case x of
        Nothing -> False
        Just _ -> True
    
validateTels :: Maybe Text -> Maybe Text -> Bool
validateTels Nothing Nothing = False
validateTels _ _ = True

validateMed :: Text -> Text -> Bool
validateMed cpf rg = valCpf && valRg
    where
    valCpf = cpfCheck cpf
    valRg = rgCheck rg

validateEnd :: Text -> Text -> Bool
validateEnd cep estado = valCep && valEst
    where
    --valPais = paisCheck $ unpack $ pais end
    valCep = cepCheck cep
    valEst = estadoCheck estado

cleanUsu :: MedReqJSON -> IO Usuario
cleanUsu usu = do
    now <- getCurrentTime
    pass <- hashPassw $ medreqPassword usu
    return $ Usuario {
        usuarioUsername     = medreqUsername usu,
        usuarioPassword     = pass,
        usuarioNome         = cleanAlphabet $ medreqNome usu,
        usuarioCpf          = cleanNumber $ medreqCpf usu,
        usuarioRg           = cleanRg,
        usuarioNasc         = medreqNasc usu,
        usuarioTipo         = "Medico",
        usuarioTelefone     = fmap cleanNumber $ medreqTelefone usu,
        usuarioCelular      = fmap cleanNumber $ medreqCelular usu,
        usuarioEmail        = medreqEmail usu,
        usuarioPais         = "BR",
        usuarioCep          = cleanNumber $ medreqCep usu,
        usuarioEstado       = medreqEstado usu,
        usuarioCidade       = medreqCidade usu,
        usuarioBairro       = medreqBairro usu,
        usuarioLogradouro   = medreqLogradouro usu,
        usuarioNumero       = medreqNumero usu,
        usuarioComplemento  = medreqComplemento usu,
        usuarioInsertedTimestamp        = now,
        usuarioLastUpdatedTimestamp     = now
    }
    where
    cleanNumber x = filterNumber x
    cleanRg = rgFormat $ medreqRg usu
    cleanAlphabet x = filterAlphabet x
    --hashPass = pack $ md5s $ Str $ unpack $ medreqPassword usu

cleanEspecMeds :: MedicoId -> [EspecializacaoId] -> IO [EspecMedico]
cleanEspecMeds medicoid especids = do
    now <- getCurrentTime
    especsf <- return $ map cleanEspecMed especids
    return $ map (\f -> f now) especsf
    where
    cleanEspecMed especid = EspecMedico medicoid especid

cleanMed :: UsuarioId -> Text -> Medico
cleanMed usuarioid crm =
    Medico {
        medicoUserid    = usuarioid,
        medicoCrm       = cleanNumber crm,
        medicoAtivo     = True
    }
    where
    cleanNumber x = filterNumber x
    


-- GET PERFIL  
optionsSingleMedicoR :: MedicoId -> Handler TypedContent
optionsSingleMedicoR _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])

getSingleMedicoR :: MedicoId -> Handler TypedContent
getSingleMedicoR medicoid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    bearer <- lookupBearerAuth
    medico <- runDB $ get404 medicoid :: Handler Medico
    emedico <- return $ Entity medicoid medico :: Handler (Entity Medico)
    --usuarioid <- return $ medicoUserid medico
    --usuario <- runDB $ get404 usuarioid :: Handler Usuario
    
    --emedico <- runDB $ getBy404 $ UniqueUserId usuarioid
    -- medicoid <- return $ entityKey emedico
    -- medico <- return $ entityVal emedico
    -- especmedics <- runDB $ selectList [EspecMedicoMedicoid ==. medicoid] [Asc EspecMedicoId]
    -- emids <- return $ map (especMedicoEspecid . entityVal) especmedics
    -- especs <- forM emids espec
    medgetjson <- createFromMed emedico
    sendStatusJSON ok200 (object ["resp" .= medgetjson])
    where
    -- espec emid = Entity (emid) <$> (runDB $ get404 emid) :: Handler (Entity Especializacao)
    --invalido = "Invalido" :: Text
    
createMedGet :: MedicoId -> Usuario -> Medico -> [Entity Especializacao] -> MedResJSON
createMedGet medicoid usuario medico especs = 
    MedResJSON {
        medresId                = medicoid,
        medresUsername          = usuarioUsername usuario,
        medresNome              = usuarioNome usuario,
        medresCpf               = usuarioCpf usuario,
        medresRg                = usuarioRg usuario,
        medresCrm               = medicoCrm medico,
        medresNasc              = usuarioNasc usuario,
        medresEspecializacoes   = map createEspec especs,
        medresTelefone          = usuarioTelefone usuario,
        medresCelular           = usuarioCelular usuario,
        medresEmail             = usuarioEmail usuario,
        medresCep               = usuarioCep usuario,
        medresEstado            = usuarioEstado usuario,
        medresCidade            = usuarioCidade usuario,
        medresBairro            = usuarioBairro usuario,
        medresLogradouro        = usuarioLogradouro usuario,
        medresNumero            = usuarioNumero usuario,
        medresComplemento       = usuarioComplemento usuario,
        medresAtivo             = medicoAtivo medico,
        medresInsertedTimestamp     = istamp,
        medresLastUpdatedTimestamp  = estamp
    }
    where
    istamp = utcToZonedTime utcTime $ usuarioInsertedTimestamp usuario
    estamp = utcToZonedTime utcTime $ usuarioLastUpdatedTimestamp usuario
    createEspec e = EspecJSON (entityKey e) (especializacaoNome $ entityVal e) Nothing
    
 
-- optionsListMedicoR :: Handler TypedContent
-- optionsListMedicoR = do
--     addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
--     addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
--     sendStatusJSON ok200 (object [])    
    
-- getListMedicoR :: Handler TypedContent
-- getListMedicoR = do
--     addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
--     addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
--     bearer <- lookupBearerAuth
--     emedicos <- runDB $ selectList [] [Asc MedicoId]
--     medgetjson <- forM emedicos createFromMed
--     sendStatusJSON ok200 (object ["resp" .= medgetjson])


createFromMed :: Entity Medico -> Handler MedResJSON
createFromMed emedico = do
    medico <- return $ entityVal emedico
    medicoid <- return $ entityKey emedico
    usuarioid <- return $ medicoUserid medico
    usuario <- runDB $ get404 usuarioid
    especmedics <- runDB $ selectList [EspecMedicoMedicoid ==. medicoid] [Asc EspecMedicoId]
    emids <- return $ map (especMedicoEspecid . entityVal) especmedics
    especs <- forM emids espec
    medgetjson <- return $ createMedGet medicoid usuario medico especs
    return $ medgetjson
    where
    espec emid = Entity (emid) <$> (runDB $ get404 emid) :: Handler (Entity Especializacao)
    
    
-- ativar / desativar

optionsDesativarMedicoR :: MedicoId -> Handler TypedContent
optionsDesativarMedicoR _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION, CONTENT-TYPE"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "PATCH"
    sendStatusJSON ok200 (object [])

patchDesativarMedicoR :: MedicoId -> Handler TypedContent
patchDesativarMedicoR medicoid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION, CONTENT-TYPE"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "PATCH"
    bearer <- lookupBearerAuth
    _ <- runDB $ get404 medicoid
    runDB $ update medicoid [MedicoAtivo =. False]
    sendStatusJSON ok200 (object ["resp" .= medicoid])


optionsAtivarMedicoR :: MedicoId -> Handler TypedContent
optionsAtivarMedicoR _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION, CONTENT-TYPE"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "PATCH"
    sendStatusJSON ok200 (object [])
    
patchAtivarMedicoR :: MedicoId -> Handler TypedContent
patchAtivarMedicoR medicoid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION, CONTENT-TYPE"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "PATCH"
    bearer <- lookupBearerAuth
    _ <- runDB $ get404 medicoid
    runDB $ update medicoid [MedicoAtivo =. True]
    sendStatusJSON ok200 (object ["resp" .= medicoid])

-- Alterar

optionsAlterarMedicoR :: MedicoId -> Handler TypedContent
optionsAlterarMedicoR _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "PUT"
    sendStatusJSON ok200 (object [])

putAlterarMedicoR :: MedicoId -> Handler TypedContent
putAlterarMedicoR medicoid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "PUT"
    bearer <- lookupBearerAuth
    medico <- runDB $ get404 medicoid
    medjson <- requireJsonBody :: Handler MedAltJSON
    isValid <- validateMedAlt medjson
    if (not isValid) then
        sendStatusJSON badRequest400 (object ["resp" .= invalido])
    else do
        usuid <- return $ medicoUserid $ medico
        usuario <- runDB $ get404 usuid
        cleanusu <- liftIO $ cleanAltUsu medjson usuario
        cleanmed <- return $ cleanAltMed usuid medjson medico
        runDB $ deleteWhere [EspecMedicoMedicoid ==. medicoid]
        cleanespecmeds <- liftIO $ cleanEspecMeds medicoid $ medaltEspecializacoes medjson
        runDB $ replace usuid cleanusu
        runDB $ replace medicoid cleanmed
        especmedics <- forM cleanespecmeds insEspecMed
        sendStatusJSON ok200 (object ["resp" .= medicoid])
    where
    invalido = "Invalido" :: Text
    insEspecMed e = runDB $ insert e :: Handler EspecMedicoId
    

validateMedAlt :: MedAltJSON -> Handler Bool
validateMedAlt medjson = do 
    validEspec <- validateEspecs (medaltEspecializacoes medjson)
    validMed <- return $ validateMed (medaltCpf medjson) (medaltRg medjson)
    validEnds <- return $ validateEnd (medaltCep medjson) (medaltEstado medjson)
    validTel <- return $ validateTels (medaltTelefone medjson) (medaltCelular medjson)
    return $ (validMed && validEnds && validTel && validEspec)    

    
cleanAltUsu :: MedAltJSON -> Usuario -> IO Usuario
cleanAltUsu usujson usu = do
    now <- getCurrentTime
    return $ Usuario {
        usuarioUsername     = usuarioUsername usu,
        usuarioPassword     = usuarioPassword usu,
        usuarioNome         = cleanAlphabet $ medaltNome usujson,
        usuarioCpf          = cleanNumber $ medaltCpf usujson,
        usuarioRg           = cleanRg,
        usuarioNasc         = medaltNasc usujson,
        usuarioTipo         = "Medico",
        usuarioTelefone     = fmap cleanNumber $ medaltTelefone usujson,
        usuarioCelular      = fmap cleanNumber $ medaltCelular usujson,
        usuarioEmail        = medaltEmail usujson,
        usuarioPais         = "BR",
        usuarioCep          = cleanNumber $ medaltCep usujson,
        usuarioEstado       = medaltEstado usujson,
        usuarioCidade       = medaltCidade usujson,
        usuarioBairro       = medaltBairro usujson,
        usuarioLogradouro   = medaltLogradouro usujson,
        usuarioNumero       = medaltNumero usujson,
        usuarioComplemento  = medaltComplemento usujson,
        usuarioInsertedTimestamp        = usuarioInsertedTimestamp usu,
        usuarioLastUpdatedTimestamp     = now
    }
    where
    cleanNumber x = filterNumber x
    cleanRg = rgFormat $ medaltRg usujson
    cleanAlphabet x = filterAlphabet x
    --hashPass = pack $ md5s $ Str $ unpack $ medreqPassword usu
    
    
cleanAltMed :: UsuarioId -> MedAltJSON -> Medico -> Medico
cleanAltMed usuarioid medjson med =
    Medico {
        medicoUserid    = usuarioid,
        medicoCrm       = cleanNumber $ medaltCrm medjson,
        medicoAtivo     = medicoAtivo med
    }
    where
    cleanNumber x = filterNumber x
    
--Busca

optionsListMedicoR :: Handler TypedContent
optionsListMedicoR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])    
    
getListMedicoR :: Handler TypedContent
getListMedicoR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    bearer <- lookupBearerAuth
    mNome <- lookupGetParam $ T.pack "nome"
    mEspec <- lookupGetParam $ T.pack "especializacao"
    nomeFilter <- return $ createMedFilterNome mNome
    especFilter <- return $ createMedFilterEspec mEspec
    
    eespecmedics <- runDB $ selectList especFilter [Asc EspecMedicoId]
    
    medicoids <- return $ map (\eem -> especMedicoMedicoid $ entityVal eem) eespecmedics
    
    emedicos' <- runDB $ selectList [MedicoId <-. medicoids, MedicoAtivo ==. True] [Asc MedicoId]
    
    usuarioids' <- return $ map (\em -> medicoUserid $ entityVal em) emedicos'
    
    eusuarios <- runDB $ selectList ([UsuarioId <-. usuarioids'] ++ nomeFilter) [Asc UsuarioId]
    
    usuarioids <- return $ map entityKey eusuarios
    
    emedicos <- runDB $ selectList [MedicoUserid <-. usuarioids] [Asc MedicoId]
    
    medgetjson <- forM emedicos createFromMed
    sendStatusJSON ok200 (object ["resp" .= medgetjson])
    

createMedFilterNome :: Maybe Text -> [Filter Usuario]
createMedFilterNome mNome = 
    case mNome of
        Just nome -> [Filter UsuarioNome (Left $ concat ["%", nome, "%"]) (BackendSpecificFilter "ILIKE")]
        Nothing -> []
        
createMedFilterEspec :: Maybe Text -> [Filter EspecMedico]
createMedFilterEspec mEspec = 
    case mEspec of
        Just espec -> case (readMaybe $ T.unpack espec :: Maybe Int64) of
            Just especint -> [EspecMedicoEspecid ==. (toSqlKey especint)]
            Nothing -> []
        Nothing -> []
