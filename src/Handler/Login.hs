{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Login where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

import Jose.Jws
import Jose.Jwa
import Jose.Jwt
import Crypto.BCrypt
import Data.ByteString.Lazy.Char8 as BSLC
import Data.ByteString.Lazy as BSL
import Data.ByteString.Char8 as BSC
import Data.ByteString as BS
import Data.Time
import Text.Read (readMaybe)
import Data.Aeson
import Data.Aeson.Casing
import Data.Text as T (pack,unpack,Text)
import Data.Time.Clock.POSIX
import Handler.Validation

data LoginPost = LoginPost {
    loginpostUsername :: Text,
    loginpostPassword :: Text
} deriving Generic

instance ToJSON LoginPost where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON LoginPost where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase
   

data JwtJSON = JwtJSON {
    jwjExp      :: POSIXTime,
    jwjId       :: UsuarioId,
    jwjNome     :: Text,
    jwjUsername :: Text,
    jwjCargo    :: Int
} deriving Generic

instance ToJSON JwtJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON JwtJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

jwtAll :: (Maybe T.Text) -> Handler (Maybe JwtJSON)
jwtAll mjwt = do
    case mjwt of
        Just jwt -> do
            eitherJwt <- return $ hmacDecode jwtKey $ BSC.pack $ T.unpack jwt
            case eitherJwt of
                Right jwtr -> do
                    maybeJwtinfo <- return $ Data.Aeson.decode $ BSL.fromStrict $ snd jwtr :: Handler (Maybe JwtJSON)
                    case maybeJwtinfo of
                        Just jwtinfo -> do
                            expdate <- return $ jwjExp jwtinfo
                            validoData <- liftIO $ checkDate expdate
                            validoUsu <- checkUsuario $ jwjId jwtinfo
                            --valido <- return $ True
                            if (validoData && validoUsu) then 
                                return $ Just $ jwtinfo
                            else
                                return $ Nothing
                        Nothing -> return $ Nothing
                Left _ -> return $ Nothing
        Nothing -> return $ Nothing
        
checkUsuario :: UsuarioId -> Handler Bool
checkUsuario usuid = do
    musu <- runDB $ get usuid
    case musu of
        Just _ -> return $ True
        Nothing -> return $ False

jwtId :: T.Text -> IO (Maybe UsuarioId)
jwtId jwt = do
    eitherJwt <- return $ hmacDecode jwtKey $ BSC.pack $ T.unpack jwt
    case eitherJwt of
        Right jwtr -> do
            maybeJwtinfo <- return $ Data.Aeson.decode $ BSL.fromStrict $ snd jwtr :: IO (Maybe JwtJSON)
            case maybeJwtinfo of
                Just jwtinfo -> do
                    expdate <- return $ jwjExp jwtinfo
                    valido <- checkDate expdate
                    if valido then 
                        return $ Just $ jwjId jwtinfo
                    else
                        return $ Nothing
                Nothing -> return $ Nothing
        Left _ -> return $ Nothing
    

jwtData :: T.Text -> IO Bool
jwtData jwt = do
    eitherJwt <- return $ hmacDecode jwtKey $ BSC.pack $ T.unpack jwt
    case eitherJwt of
        Right jwtr -> do
            maybeJwtinfo <- return $ Data.Aeson.decode $ BSL.fromStrict $ snd jwtr :: IO (Maybe JwtJSON)
            case maybeJwtinfo of
                Just jwtinfo -> do
                    expdate <- return $ jwjExp jwtinfo
                    valido <- checkDate expdate
                    if valido then 
                        return $ True
                    else
                        return $ False
                Nothing -> return $ False
        Left _ -> return $ False

jwtTipo :: T.Text -> IO (Maybe Int)
jwtTipo jwt = do
    eitherJwt <- return $ hmacDecode jwtKey $ BSC.pack $ T.unpack jwt
    case eitherJwt of
        Right jwtr -> do
            maybeJwtinfo <- return $ Data.Aeson.decode $ BSL.fromStrict $ snd jwtr :: IO (Maybe JwtJSON)
            case maybeJwtinfo of
                Just jwtinfo -> do
                    expdate <- return $ jwjExp jwtinfo
                    valido <- checkDate expdate
                    if valido then 
                        return $ Just $ jwjCargo jwtinfo
                    else
                        return $ Nothing
                Nothing -> return $ Nothing
        Left _ -> return $ Nothing


checkDate :: POSIXTime -> IO Bool
checkDate expdate = do
    expire <- return $ posixSecondsToUTCTime expdate
    now <- getCurrentTime
    return (expire >= now)


makeJwtClaims :: UsuarioId -> Text -> Text -> Int -> IO JwtJSON
makeJwtClaims usuarioid nome username tipo = do
  currentUTC <- getCurrentTime
  let laterDate = utcTimeToPOSIXSeconds $ addUTCTime (60 * 60 * 16) currentUTC
  return $
    JwtJSON {
        jwjExp      = laterDate,
        jwjId       = usuarioid,
        jwjNome     = nome,
        jwjUsername = username,
        jwjCargo     = tipo
    }

            
jwtKey :: BS.ByteString
jwtKey = "wkjfhfkuheuihgKUGYKJGYrekjygKJefgkG54W5E1FGKAUHWEFKJBfkjWBEJEFS35EWG458415"


postLoginR :: Handler TypedContent
postLoginR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    loginjson <- requireJsonBody :: Handler LoginPost
    lusrname <- return $ filterAlphaNumber $ loginpostUsername loginjson
    lpssword <- return $ loginpostPassword loginjson
    mEusuario <- runDB $ getBy $ UniqueUserUsername lusrname
    case mEusuario of
        Just eusuario -> do
            --mEmedico <- runDB $ getBy $ UniqueUserId $ entityKey eusuario
            --mAtivo <- return $ medicoAtivo <$> (entityVal <$> mEmedico)
            --isAtivo <- return $ maybeBool mAtivo
            bpssword <- return $ BSC.pack $ T.unpack lpssword
            hpssword <- return $ BSC.pack $ T.unpack $ usuarioPassword $ entityVal eusuario
            if (validatePassword hpssword bpssword) then do
                tipo <- return $ tipoInt $ usuarioTipo $ entityVal eusuario
                username <- return $ usuarioUsername $ entityVal eusuario
                usuarioid <- return $ entityKey eusuario
                nome <- return $ usuarioNome $ entityVal eusuario
                info <- liftIO $ makeJwtClaims usuarioid nome username tipo
                infojson <- return $ BSL.toStrict $ Data.Aeson.encode info
                eitherJwt <- return $ hmacEncode HS384 jwtKey infojson
                case eitherJwt of
                    Right jwt -> do
                        jwtT <- return $ unJwt jwt
                        jwts <- return $ BSC.unpack jwtT
                        sendStatusJSON ok200 (object ["jwt" .= jwts])
                    Left _ -> do
                        sendStatusJSON unauthorized401 (object [])
            else
                sendStatusJSON unauthorized401 (object ["resp" .= T.pack "Senha Inválida"])
        Nothing -> do
            sendStatusJSON unauthorized401 (object ["resp" .= T.pack "Usuário Inválido"])
    where
    tipoInt tp = case tp of
        "Admin"         -> 1
        "Secretaria"    -> 2
        "Medico"        -> 3
    --maybeBool (Just True) = True
    --maybeBool (Just False) = False
    --maybeBool Nothing = True
        
        

getLoginR :: Handler TypedContent
getLoginR = do
    addHeader "Access-Control-Allow-Origin" "*"
    usuarioid <- return $ toSqlKey $ 1
    info <- liftIO $ makeJwtClaims usuarioid "Outro nome..." "um_nome_ai" 1
    infojson <- return $ BSL.toStrict $ Data.Aeson.encode info
    rjwt <- return $ hmacEncode HS384 jwtKey infojson
    case rjwt of
        Right jwt -> do
            jwtk <- return $ unJwt jwt
            jwtks <- return $ BSC.unpack jwtk
            jwtt <- return $ T.pack jwtks
            mbearer <- return $ Just jwtt
            mjwtInfo <- jwtAll mbearer
            case mjwtInfo of
                Just jwtInfo -> do
                    now <- liftIO $ getCurrentTime
                    expdate <- return $ posixSecondsToUTCTime $ jwjExp jwtInfo
                    sendStatusJSON ok200 (object ["now" .= now,"expdate" .= expdate,"expired" .= (now > expdate)])
                Nothing -> sendStatusJSON unauthorized401 (object ["bearer" .= mbearer])
        Left _ -> do
            sendStatusJSON notFound404 (object [])
