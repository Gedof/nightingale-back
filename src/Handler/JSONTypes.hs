{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.JSONTypes where

import Import
import Data.Time
import Data.Aeson
import Data.Aeson.Casing


data Pais = BR deriving (Show,Read,Enum,Eq,Generic)

instance ToJSON Pais
instance FromJSON Pais


data Estado =   AC | AL | AP | AM | BA | CE | DF | ES | GO | MA | MT | MS | MG | PA |
                PB | PR | PE | PI | RJ | RN | RS | RO | RR | SC | SP | SE | TO 
                deriving (Show,Read,Enum,Eq,Generic)
                
                


instance ToJSON Estado
instance FromJSON Estado


data PacCadJSON = PacCadJSON {
    paccadPaciente    :: PacReqJSON
} deriving (Show, Read, Generic)

instance ToJSON PacCadJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON PacCadJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase


data PacReqJSON = PacReqJSON {
    pacreqNome          :: Text,
    pacreqCpf           :: Text,
    pacreqRg            :: Text,
    pacreqNasc          :: Day,
    pacreqTelefone      :: Maybe Text,
    pacreqCelular       :: Maybe Text,
    pacreqEmail         :: Text,
    pacreqCep           :: Text,
    pacreqEstado        :: Text,
    pacreqCidade        :: Text,
    pacreqBairro        :: Text,
    pacreqLogradouro    :: Text,
    pacreqNumero        :: Text,
    pacreqComplemento   :: Maybe Text
} deriving (Show, Read, Generic)

instance ToJSON PacReqJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON PacReqJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase


--PacRes

data PacGetJSON = PacGetJSON {
    pacgetPaciente    :: PacResJSON
} deriving (Show, Read, Generic)

instance ToJSON PacGetJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON PacGetJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase


data PacResJSON = PacResJSON {
    pacresId            :: PacienteId,
    pacresNome          :: Text,
    pacresCpf           :: Text,
    pacresRg            :: Text,
    pacresNasc          :: Day,
    pacresTelefone      :: Maybe Text,
    pacresCelular       :: Maybe Text,
    pacresEmail         :: Text,
    pacresCep           :: Text,
    pacresEstado        :: Text,
    pacresCidade        :: Text,
    pacresBairro        :: Text,
    pacresLogradouro    :: Text,
    pacresNumero        :: Text,
    pacresComplemento   :: Maybe Text,
    pacresInsertedTimestamp     :: ZonedTime,
    pacresLastUpdatedTimestamp  :: ZonedTime
} deriving (Show, Read, Generic)

instance ToJSON PacResJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON PacResJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase


--FunReq

data FunCadJSON = FunCadJSON {
    funcadFuncionario   :: FunReqJSON
} deriving (Show, Read, Generic)

instance ToJSON FunCadJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON FunCadJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase


data FunReqJSON = FunReqJSON {
    funreqUsername      :: Text,
    funreqPassword      :: Text,
    funreqNome          :: Text,
    funreqCpf           :: Text,
    funreqRg            :: Text,
    funreqNasc          :: Day,
    funreqCargo         :: Int,
    funreqTelefone      :: Maybe Text,
    funreqCelular       :: Maybe Text,
    funreqEmail         :: Text,
    funreqCep           :: Text,
    funreqEstado        :: Text,
    funreqCidade        :: Text,
    funreqBairro        :: Text,
    funreqLogradouro    :: Text,
    funreqNumero        :: Text,
    funreqComplemento   :: Maybe Text
} deriving (Show, Read, Generic)

instance ToJSON FunReqJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON FunReqJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase
   
   
--FunAlt


data FunAltJSON = FunAltJSON {
    funaltNome          :: Text,
    funaltCpf           :: Text,
    funaltRg            :: Text,
    funaltNasc          :: Day,
    funaltCargo         :: Int,
    funaltTelefone      :: Maybe Text,
    funaltCelular       :: Maybe Text,
    funaltEmail         :: Text,
    funaltCep           :: Text,
    funaltEstado        :: Text,
    funaltCidade        :: Text,
    funaltBairro        :: Text,
    funaltLogradouro    :: Text,
    funaltNumero        :: Text,
    funaltComplemento   :: Maybe Text
} deriving (Show, Read, Generic)

instance ToJSON FunAltJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON FunAltJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase
   

--FunRes

data FunGetJSON = FunGetJSON {
    fungetFuncionario   :: FunResJSON
} deriving (Show, Read, Generic)

instance ToJSON FunGetJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON FunGetJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase


data FunResJSON = FunResJSON {
    funresId            :: UsuarioId,
    funresUsername      :: Text,
    funresNome          :: Text,
    funresCpf           :: Text,
    funresRg            :: Text,
    funresNasc          :: Day,
    funresCargo         :: Int,
    funresTelefone      :: Maybe Text,
    funresCelular       :: Maybe Text,
    funresEmail         :: Text,
    funresCep           :: Text,
    funresEstado        :: Text,
    funresCidade        :: Text,
    funresBairro        :: Text,
    funresLogradouro    :: Text,
    funresNumero        :: Text,
    funresComplemento   :: Maybe Text,
    funresInsertedTimestamp     :: ZonedTime,
    funresLastUpdatedTimestamp  :: ZonedTime
} deriving (Show, Read, Generic)

instance ToJSON FunResJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON FunResJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

utcTime :: TimeZone
utcTime = TimeZone (0) False "UTC"
   
   
--MedReq

data MedCadJSON = MedCadJSON {
    medcadMedico  :: MedReqJSON
} deriving (Show, Read, Generic)

instance ToJSON MedCadJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON MedCadJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase


data MedReqJSON = MedReqJSON {
    medreqUsername          :: Text,
    medreqPassword          :: Text,
    medreqNome              :: Text,
    medreqCpf               :: Text,
    medreqRg                :: Text,
    medreqCrm               :: Text,
    medreqNasc              :: Day,
    medreqEspecializacoes   :: [EspecializacaoId],
    medreqTelefone          :: Maybe Text,
    medreqCelular           :: Maybe Text,
    medreqEmail             :: Text,
    medreqCep               :: Text,
    medreqEstado            :: Text,
    medreqCidade            :: Text,
    medreqBairro            :: Text,
    medreqLogradouro        :: Text,
    medreqNumero            :: Text,
    medreqComplemento       :: Maybe Text
} deriving (Show, Read, Generic)

instance ToJSON MedReqJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON MedReqJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase
   
 
 --MedAlt
 
data MedAltJSON = MedAltJSON {
    medaltNome              :: Text,
    medaltCpf               :: Text,
    medaltRg                :: Text,
    medaltCrm               :: Text,
    medaltNasc              :: Day,
    medaltEspecializacoes   :: [EspecializacaoId],
    medaltTelefone          :: Maybe Text,
    medaltCelular           :: Maybe Text,
    medaltEmail             :: Text,
    medaltCep               :: Text,
    medaltEstado            :: Text,
    medaltCidade            :: Text,
    medaltBairro            :: Text,
    medaltLogradouro        :: Text,
    medaltNumero            :: Text,
    medaltComplemento       :: Maybe Text
} deriving (Show, Read, Generic)

instance ToJSON MedAltJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON MedAltJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase
 
   
   
--MedRes

data MedGetJSON = MedGetJSON {
    medgetMedico  :: MedResJSON
} deriving (Show, Read, Generic)

instance ToJSON MedGetJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON MedGetJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase


data MedResJSON = MedResJSON {
    medresId                :: MedicoId,
    medresUsername          :: Text,
    medresNome              :: Text,
    medresCpf               :: Text,
    medresRg                :: Text,
    medresCrm               :: Text,
    medresNasc              :: Day,
    medresEspecializacoes   :: [EspecJSON],
    medresTelefone          :: Maybe Text,
    medresCelular           :: Maybe Text,
    medresEmail             :: Text,
    medresCep               :: Text,
    medresEstado            :: Text,
    medresCidade            :: Text,
    medresBairro            :: Text,
    medresLogradouro        :: Text,
    medresNumero            :: Text,
    medresComplemento       :: Maybe Text,
    medresAtivo             :: Bool,
    medresInsertedTimestamp     :: ZonedTime,
    medresLastUpdatedTimestamp  :: ZonedTime
} deriving (Show, Read, Generic)

instance ToJSON MedResJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON MedResJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase
   

--ConsReq

data ConsultaCadJSON = ConsultaCadJSON {
    conscadConsulta :: ConsReqJSON
} deriving (Show, Read, Generic)

instance ToJSON ConsultaCadJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON ConsultaCadJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase  


data ConsReqJSON = ConsReqJSON {
    consreqPacienteid  :: PacienteId,
    consreqMedicoid    :: MedicoId,
    consreqEspecid     :: EspecializacaoId,
    consreqInicio      :: ZonedTime,
    consreqTermino     :: ZonedTime,
    consreqObservacoes :: Text
} deriving (Show, Read, Generic)
   
instance ToJSON ConsReqJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON ConsReqJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase
   
   
--ConsRes

data ConsultaGetJSON = ConsultaGetJSON {
    consgetConsulta :: ConsResJSON
} deriving (Show, Read, Generic)

instance ToJSON ConsultaGetJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON ConsultaGetJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase  


data ConsResJSON = ConsResJSON {
    consresId           :: ConsultaId,
    consresPacienteid   :: PacienteId,
    consresMedicoid     :: MedicoId,
    consresEspecid      :: EspecializacaoId,
    consresInicio       :: ZonedTime,
    consresTermino      :: ZonedTime,
    consresObservacoes  :: Text,
    consresInsertedTimestamp    :: ZonedTime,
    consresLastUpdatedTimestamp :: ZonedTime
} deriving (Show, Read, Generic)
   
instance ToJSON ConsResJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON ConsResJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase  
   
   
--ProntReq

data ProntCadJSON = ProntCadJSON {
    prontcadEntradaProntuario :: ProntReqJSON
} deriving (Show, Read, Generic)
   
instance ToJSON ProntCadJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON ProntCadJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase
   

data ProntReqJSON = ProntReqJSON {
    prontreqPacienteid :: PacienteId,
    prontreqMedicoid   :: Maybe MedicoId,
    prontreqEspecid    :: Maybe EspecializacaoId,
    prontreqConteudo   :: Text
} deriving (Show, Read, Generic)
   
instance ToJSON ProntReqJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON ProntReqJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase
   
   
--ProntRes

data ProntGetJSON = ProntGetJSON {
    prontgetEntradaProntuario :: ProntResJSON
} deriving (Show, Read, Generic)
   
instance ToJSON ProntGetJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON ProntGetJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase
   

data ProntResJSON = ProntResJSON {
    prontresId          :: EntradaProntuarioId,
    prontresPacienteid  :: PacienteId,
    prontresMedicoid    :: Maybe MedicoId,
    prontresEspecid     :: Maybe EspecializacaoId,
    prontresConteudo    :: Text,
    prontresTimestamp   :: ZonedTime
} deriving (Show, Read, Generic)
   
instance ToJSON ProntResJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON ProntResJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase  
   
   
   
   
--Espec

data EspecsJSON = EspecsJSON {
    especsEspecializacoes   :: [EspecJSON]
} deriving (Show, Read, Generic)

instance ToJSON EspecsJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON EspecsJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

data EspecJSON = EspecJSON {
    especId         :: EspecializacaoId,
    especNome       :: Text,
    especDescricao  :: Maybe Text
} deriving (Show, Read, Generic)

instance ToJSON EspecJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON EspecJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase