{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Email where

import Import
import Data.Time
import Handler.Validation
import Handler.JSONTypes
import Data.ByteString.Char8 as BS (pack, unpack)
import Data.ByteString.Lazy as BSL (toStrict)
import Data.Text as T (pack,unpack,Text)
import Mail.Hailgun
import Handler.Login
import Test.RandomStrings
import Data.Aeson
import Data.Aeson.Casing
import Network.HTTP.Simple as NHS


data EmailJSON = EmailJSON {
    emailEmail :: Text,
    emailSenha :: Text
} deriving Generic

instance ToJSON EmailJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON EmailJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase


mailgunDomain :: String
mailgunDomain = "sandboxbe25ba7eacd14a2c8fbb6d813bdb312b.mailgun.org"


mailgunApiKey :: String
mailgunApiKey = "9727806ecf02fe1d6affa4b4686bd5af-4412457b-bb8b0a29"


mailgunReplyAddress :: String
mailgunReplyAddress = "nightingaleclinic@sandboxbe25ba7eacd14a2c8fbb6d813bdb312b.mailgun.org"



createAndSendEmail :: ByteString -> ByteString -> IO String
createAndSendEmail email senha = do
    domain <- return $ mailgunDomain
    apiKey <- return $ mailgunApiKey
    replyAddress <- return $ BS.pack mailgunReplyAddress
    let context = HailgunContext domain apiKey Nothing
    let msg = mkMessage replyAddress
    case msg of
        Left err -> return $ ("Making failed: " ++ show err)
        Right msg' -> do
            result <- sendEmail context msg'
            case result of
                Left err -> return $ ("Sending failed: " ++ show err)
                Right resp -> return $ ("Sending succeeded: " ++ show resp)
    where
    mkMessage replyAddress = hailgunMessage
        "Alterar Senha"
        (TextOnly $ "Sua nova senha: " ++ senha)
        replyAddress
        (emptyMessageRecipients { recipientsTo = [email] } )
        []
  

optionsRedefinirSenhaR :: Text -> Handler TypedContent
optionsRedefinirSenhaR _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION, CONTENT-TYPE"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "PATCH"
    sendStatusJSON ok200 (object [])

patchRedefinirSenhaR :: Text -> Handler TypedContent
patchRedefinirSenhaR username = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION, CONTENT-TYPE"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "PATCH"
    eusuario <- runDB $ getBy404 $ UniqueUserUsername username
    usuarioid <- return $ entityKey eusuario
    email <- return $ usuarioEmail $ entityVal eusuario
    novasenha <- liftIO $ randomString (onlyAlphaNum randomASCII) 10
    hsenha <- liftIO $ hashPassw $ T.pack novasenha
    runDB $ update usuarioid [UsuarioPassword =. hsenha]
    bemail <- return $ BS.pack $ T.unpack email
    bsenha <- return $ BS.pack $ novasenha
    res <- NHS.httpLBS
        $ NHS.setRequestBodyJSON (EmailJSON email $ T.pack novasenha)
        $ NHS.parseRequest_ "POST http://brunotcc.dreamhosters.com/wp-json/clinichead/v1/mandaremail1234"
    --res <- liftIO $ createAndSendEmail bemail bsenha
    --sendStatusJSON ok200 (object ["resp" .= (BS.unpack $ BSL.toStrict $ responseBody res)])
    sendStatusJSON ok200 (decode $ responseBody res :: Maybe Object)

data AltSenhaJSON = AltSenhaJSON {
    senhaPassword   :: Text,
    senhaPassword2  :: Text
} deriving Generic

instance ToJSON AltSenhaJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON AltSenhaJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase


optionsAlterarSenhaR :: Handler TypedContent
optionsAlterarSenhaR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION, CONTENT-TYPE"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "PATCH"
    sendStatusJSON ok200 (object [])

patchAlterarSenhaR :: Handler TypedContent
patchAlterarSenhaR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION, CONTENT-TYPE"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "PATCH"
    mbearer <- lookupBearerAuth
    senhajson <- requireJsonBody :: Handler AltSenhaJSON
    --prontjson <- return $ prontcadEntradaProntuario prontcadjson
    mjwtInfo <- jwtAll mbearer
    case mjwtInfo of
        Just jwtInfo -> do
            isValid <- return $ (senhaPassword senhajson == senhaPassword2 senhajson)
            usuid <- return $ jwjId jwtInfo
            if (not isValid) then
                sendStatusJSON badRequest400 (object ["resp" .= invalido])
            else do
                _ <- runDB $ get404 usuid
                hsenha <- liftIO $ hashPassw $ senhaPassword senhajson
                runDB $ update usuid [UsuarioPassword =. hsenha]
                sendStatusJSON ok200 (object ["resp" .= usuid])
        Nothing -> sendStatusJSON unauthorized401 (object ["bearer" .= mbearer])
    where
        invalido = "Invalido" :: Text

    
getEmailR :: Handler TypedContent
getEmailR = do
    --teste <- liftIO $ createAndSendEmail
    teste <- liftIO $ randomString (onlyAlphaNum randomASCII) 10
    sendStatusJSON ok200 (object ["resp" .= [teste,teste,teste]])