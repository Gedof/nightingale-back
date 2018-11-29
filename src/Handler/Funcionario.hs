{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Funcionario where

import Import
import Data.Time
import Handler.Validation
import Handler.JSONTypes
import Data.Text as T (pack,unpack,Text)
import Handler.Login
--import Data.Hash.MD5


--POST FUNCIONARIO
optionsFuncionarioR :: Handler TypedContent
optionsFuncionarioR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])

postFuncionarioR :: Handler TypedContent
postFuncionarioR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    mbearer <- lookupBearerAuth
    mjwtInfo <- jwtAll mbearer
    case mjwtInfo of
        Just jwtInfo -> do
            case (jwjCargo jwtInfo) of
                x | elem x [1] -> do
                    funjson <- requireJsonBody :: Handler FunReqJSON
                    --funjson <- return $ funcadFuncionario funcadjson
                    isValid <- return $ validateFunCad funjson
                    if (not isValid) then
                        sendStatusJSON badRequest400 (object ["resp" .= invalido])
                    else do
                        cleanfun <- liftIO $ cleanFuncionario funjson
                        funcionarioid <- runDB $ insert cleanfun
                        sendStatusJSON created201 (object ["resp" .= funcionarioid])
                _ -> sendStatusJSON forbidden403 (object ["resp" .= (1::Int)])
        Nothing -> sendStatusJSON unauthorized401 (object ["resp" .= (1::Int)])
    where
    invalido = "Invalido" :: Text
    
    
validateFunCad :: FunReqJSON -> Bool
validateFunCad funjson = validFun && validEnds && validTel && validCar
    where
    validFun = validateFun (funreqCpf funjson) (funreqRg funjson)
    validEnds = validateEnd (funreqCep funjson) (funreqEstado funjson)
    validTel = validateTels (funreqTelefone funjson) (funreqCelular funjson)
    validCar = elem (funreqCargo funjson) [1,2]
    
validateTels :: Maybe Text -> Maybe Text -> Bool
validateTels Nothing Nothing = False
validateTels _ _ = True

validateFun :: Text -> Text -> Bool
validateFun cpf rg = valCpf && valRg
    where
    valCpf = cpfCheck cpf
    valRg = rgCheck rg

validateEnd :: Text -> Text -> Bool
validateEnd cep estado = valCep && valEst
    where
    --valPais = paisCheck $ unpack $ pais end
    valCep = cepCheck cep
    valEst = estadoCheck estado


cleanFuncionario :: FunReqJSON -> IO Usuario
cleanFuncionario fun = do
    now <- getCurrentTime
    pass <- hashPassw $ funreqPassword fun
    return $ Usuario {
        usuarioUsername     = funreqUsername fun,
        usuarioPassword     = pass,
        usuarioNome         = cleanAlphabet $ funreqNome fun,
        usuarioCpf          = cleanNumber $ funreqCpf fun,
        usuarioRg           = cleanRg,
        usuarioNasc         = funreqNasc fun,
        usuarioTipo         = cleanTipo,
        usuarioTelefone     = fmap cleanNumber $ funreqTelefone fun,
        usuarioCelular      = fmap cleanNumber $ funreqCelular fun,
        usuarioEmail        = funreqEmail fun,
        usuarioPais         = "BR",
        usuarioCep          = cleanNumber $ funreqCep fun,
        usuarioEstado       = funreqEstado fun,
        usuarioCidade       = funreqCidade fun,
        usuarioBairro       = funreqBairro fun,
        usuarioLogradouro   = funreqLogradouro fun,
        usuarioNumero       = funreqNumero fun,
        usuarioComplemento  = funreqComplemento fun,
        usuarioInsertedTimestamp        = now,
        usuarioLastUpdatedTimestamp     = now
    }
    where
    cleanNumber x = filterNumber x
    cleanRg = rgFormat $ funreqRg fun
    cleanAlphabet x = filterAlphabet x
    cleanTipo = case (funreqCargo fun) of
        1 -> "Admin"        :: Text
        2 -> "Secretaria"   :: Text
        _ -> "Secretaria"   :: Text
    --hashPass = pack $ md5s $ Str $ unpack $ funreqPassword fun
    


--GET PERFIL  
optionsSingleFuncionarioR :: UsuarioId -> Handler TypedContent
optionsSingleFuncionarioR _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])

getSingleFuncionarioR :: UsuarioId -> Handler TypedContent
getSingleFuncionarioR funcionarioid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    mbearer <- lookupBearerAuth
    mjwtInfo <- jwtAll mbearer
    case mjwtInfo of
        Just jwtInfo -> do
            case (jwjCargo jwtInfo) of
                x | elem x [1,2,3] -> do
                    funcionario <- runDB $ get404 funcionarioid :: Handler Usuario
                    if ((usuarioTipo funcionario) == "Medico") then do
                        sendStatusJSON badRequest400 (object ["resp" .= invalido])
                    else do
                        fungetjson <- return $ createFunGet funcionarioid funcionario
                        sendStatusJSON ok200 (object ["resp" .= fungetjson])
                _ -> sendStatusJSON forbidden403 (object ["resp" .= (1::Int)])
        Nothing -> sendStatusJSON unauthorized401 (object ["resp" .= (1::Int)])
    where
    invalido = "Invalido" :: Text
    
createFunGet :: UsuarioId -> Usuario -> FunResJSON
createFunGet funcionarioid fun =
    FunResJSON {
        funresId            = funcionarioid,
        funresUsername      = usuarioUsername fun,
        funresNome          = usuarioNome fun,
        funresCpf           = usuarioCpf fun,
        funresRg            = usuarioRg fun,
        funresNasc          = usuarioNasc fun,
        funresCargo         = tipo,
        funresTelefone      = usuarioTelefone fun,
        funresCelular       = usuarioCelular fun,
        funresEmail         = usuarioEmail fun,
        funresCep           = usuarioCep fun,
        funresEstado        = usuarioEstado fun,
        funresCidade        = usuarioCidade fun,
        funresBairro        = usuarioBairro fun,
        funresLogradouro    = usuarioLogradouro fun,
        funresNumero        = usuarioNumero fun,
        funresComplemento   = usuarioComplemento fun,
        funresInsertedTimestamp     = istamp,
        funresLastUpdatedTimestamp  = estamp
    }
    where
    istamp = utcToZonedTime utcTime $ usuarioInsertedTimestamp fun
    estamp = utcToZonedTime utcTime $ usuarioLastUpdatedTimestamp fun
    tipo = case (usuarioTipo fun) of
        "Admin" -> 1
        "Secretaria" -> 2
        _ -> 2


-- optionsListFuncionarioR :: Handler TypedContent
-- optionsListFuncionarioR = do
--     addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
--     addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
--     sendStatusJSON ok200 (object [])

-- getListFuncionarioR :: Handler TypedContent
-- getListFuncionarioR = do
--     addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
--     addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
--     bearer <- lookupBearerAuth
--     efuncionarios <- runDB $ selectList [UsuarioTipo !=. "Medico"] [Asc UsuarioId]
--     funcionarios <- return $ map createFunGetE efuncionarios
--     sendStatusJSON ok200 (object ["resp" .= funcionarios])
    
createFunGetE :: Entity Usuario -> FunResJSON
createFunGetE efuncionario = createFunGet funcionarioid funcionario
    where
    funcionario = entityVal efuncionario
    funcionarioid = entityKey efuncionario
    

--Delete
optionsApagarFuncionarioR :: UsuarioId -> Handler TypedContent
optionsApagarFuncionarioR _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "DELETE"
    sendStatusJSON ok200 (object [])


deleteApagarFuncionarioR :: UsuarioId -> Handler TypedContent
deleteApagarFuncionarioR usuarioid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "DELETE"
    mbearer <- lookupBearerAuth
    mjwtInfo <- jwtAll mbearer
    case mjwtInfo of
        Just jwtInfo -> do
            case (jwjCargo jwtInfo) of
                x | elem x [1] -> do
                    funcionario <- runDB $ get404 usuarioid
                    if ((usuarioTipo funcionario) == "Medico") then
                        sendStatusJSON badRequest400 (object ["resp" .= invalido])
                    else do
                        runDB $ delete usuarioid
                        sendStatusJSON ok200 (object ["resp" .= (1::Int)])
                _ -> sendStatusJSON forbidden403 (object ["resp" .= (1::Int)])
        Nothing -> sendStatusJSON unauthorized401 (object ["resp" .= (1::Int)])
    where
    invalido = "Invalido" :: Text
    
--Alterar
optionsAlterarFuncionarioR :: UsuarioId -> Handler TypedContent
optionsAlterarFuncionarioR _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "PUT"
    sendStatusJSON ok200 (object [])

putAlterarFuncionarioR :: UsuarioId -> Handler TypedContent
putAlterarFuncionarioR usuarioid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "PUT"
    mbearer <- lookupBearerAuth
    mjwtInfo <- jwtAll mbearer
    case mjwtInfo of
        Just jwtInfo -> do
            case (jwjCargo jwtInfo) of
                x | elem x [1] -> do
                    funcionario <- runDB $ get404 usuarioid
                    funjson <- requireJsonBody :: Handler FunAltJSON
                    isValid <- return $ validateFunAlt funjson
                    if (((usuarioTipo funcionario) == "Medico") || (not isValid)) then
                        sendStatusJSON badRequest400 (object ["resp" .= invalido])
                    else do
                        cleanfun <- liftIO $ cleanAltFuncionario funjson funcionario
                        runDB $ replace usuarioid cleanfun
                        sendStatusJSON ok200 (object ["resp" .= usuarioid])
                _ -> sendStatusJSON forbidden403 (object ["resp" .= (1::Int)])
        Nothing -> sendStatusJSON unauthorized401 (object ["resp" .= (1::Int)])
    where
    invalido = "Invalido" :: Text
  
validateFunAlt :: FunAltJSON -> Bool
validateFunAlt funjson = validFun && validEnds && validTel && validCar
    where
    validFun = validateFun (funaltCpf funjson) (funaltRg funjson)
    validEnds = validateEnd (funaltCep funjson) (funaltEstado funjson)
    validTel = validateTels (funaltTelefone funjson) (funaltCelular funjson)
    validCar = elem (funaltCargo funjson) [1,2]
    
    
cleanAltFuncionario :: FunAltJSON -> Usuario -> IO Usuario
cleanAltFuncionario fun usu = do
    now <- getCurrentTime
    return $ Usuario {
        usuarioUsername     = usuarioUsername usu,
        usuarioPassword     = usuarioPassword usu,
        usuarioNome         = cleanAlphabet $ funaltNome fun,
        usuarioCpf          = cleanNumber $ funaltCpf fun,
        usuarioRg           = cleanRg,
        usuarioNasc         = funaltNasc fun,
        usuarioTipo         = cleanTipo,
        usuarioTelefone     = fmap cleanNumber $ funaltTelefone fun,
        usuarioCelular      = fmap cleanNumber $ funaltCelular fun,
        usuarioEmail        = funaltEmail fun,
        usuarioPais         = "BR",
        usuarioCep          = cleanNumber $ funaltCep fun,
        usuarioEstado       = funaltEstado fun,
        usuarioCidade       = funaltCidade fun,
        usuarioBairro       = funaltBairro fun,
        usuarioLogradouro   = funaltLogradouro fun,
        usuarioNumero       = funaltNumero fun,
        usuarioComplemento  = funaltComplemento fun,
        usuarioInsertedTimestamp        = usuarioInsertedTimestamp usu,
        usuarioLastUpdatedTimestamp     = now
    }
    where
    cleanNumber x = filterNumber x
    cleanRg = rgFormat $ funaltRg fun
    cleanAlphabet x = filterAlphabet x
    cleanTipo = case (funaltCargo fun) of
        1 -> "Admin"        :: Text
        2 -> "Secretaria"   :: Text
        _ -> "Secretaria"   :: Text
    --hashPass = pack $ md5s $ Str $ unpack $ funreqPassword fun
    

--Busca

optionsListFuncionarioR :: Handler TypedContent
optionsListFuncionarioR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])

getListFuncionarioR :: Handler TypedContent
getListFuncionarioR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    mbearer <- lookupBearerAuth
    mjwtInfo <- jwtAll mbearer
    case mjwtInfo of
        Just jwtInfo -> do
            case (jwjCargo jwtInfo) of
                x | elem x [1,2,3] -> do
                    mNome <- lookupGetParam $ T.pack "nome"
                    mCargo <- lookupGetParam $ T.pack "cargo"
                    nomeFilter <- return $ createFuncFilterNome mNome
                    rgFilter <- return $ createFuncFilterCargo mCargo
                    efuncionarios <- runDB $ selectList (concat [[UsuarioTipo !=. "Medico"],nomeFilter,rgFilter]) [Asc UsuarioId]
                    funcionarios <- return $ map createFunGetE efuncionarios
                    sendStatusJSON ok200 (object ["resp" .= funcionarios])
                _ -> sendStatusJSON forbidden403 (object ["resp" .= (1::Int)])
        Nothing -> sendStatusJSON unauthorized401 (object ["resp" .= (1::Int), "bearer" .= mbearer, "jwt" .= mjwtInfo])
    
    
createFuncFilterNome :: Maybe Text -> [Filter Usuario]
createFuncFilterNome mNome = 
    case mNome of
        Just nome -> [Filter UsuarioNome (Left $ concat ["%", nome, "%"]) (BackendSpecificFilter "ILIKE")]
        Nothing -> []
        
createFuncFilterCargo :: Maybe Text -> [Filter Usuario]
createFuncFilterCargo mCargo = 
    case mCargo of
        Just "1" -> [UsuarioTipo ==. "Admin"]
        Just "2" -> [UsuarioTipo ==. "Secretaria"]
        Just _ -> [UsuarioTipo ==. "Nada"]
        Nothing -> []