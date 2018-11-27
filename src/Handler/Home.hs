{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Home where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

import Jose.Jws
import Jose.Jwa
import Jose.Jwt
import Crypto.Hash
import Data.ByteString.Lazy.Char8
import Data.ByteString.Char8
import Data.Time
import Text.Read
import Data.Aeson
import Data.Aeson.Casing

--instance ToJSON Import.ByteString
--instance FromJSON Import.ByteString

data TimeJSON = TimeJSON {timejsonTempo::ZonedTime} deriving (Show,Read,Generic)

instance ToJSON TimeJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON TimeJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

getHomeR :: Handler TypedContent
getHomeR = do
    addHeader "Access-Control-Allow-Origin" "*"
    rjwt <- return $ hmacEncode HS384 "somehmackey" "my JSON message"
    case rjwt of
        Right jwt -> do
            jwtk <- return $ unJwt jwt
            jwtd <- return $ hmacDecode "somehmackey" $ (Data.ByteString.Char8.pack $ show jwtk)
            oi3 <- return $ (hashlazy (Data.ByteString.Lazy.Char8.pack $ show jwt) :: Digest MD5)
            now <- liftIO $ getCurrentTime
            --agora <- return $ (Data.Aeson.decode "{\"tempo\":\"2018-10-30T17:00-0300\"}" :: Maybe TimeJSON)
            agora <- return $ TimeJSON $ utcToZonedTime brTime now
            sendStatusJSON ok200 (object ["BRT" .= agora,"UTC" .= now])
        Left jwt -> do
            sendStatusJSON notFound404 (object ["resp" .= (show "huh")])
            
brTime :: TimeZone
brTime = TimeZone (-180) False "BRT"
