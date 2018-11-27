{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Validation where 

import Prelude
import Data.Char
import Data.List
import Text.Read
import Handler.JSONTypes
import Crypto.BCrypt
import Data.ByteString.Char8 as BSC (pack,unpack,ByteString)
import Data.Text as T (pack,unpack,Text)


hashPassw :: T.Text -> IO T.Text
hashPassw pass = do 
    maybepass <- hashPasswordUsingPolicy fastBcryptHashingPolicy $ BSC.pack $ T.unpack pass
    case maybepass of
        Just hpass -> do
            return $ T.pack $ BSC.unpack hpass
        Nothing -> return $ T.pack $ "erro"


filterNumber :: T.Text -> T.Text
filterNumber s = T.pack $ filter (`elem` ['0'..'9']) $ T.unpack s

rgFormat :: T.Text -> T.Text
rgFormat rg = T.pack $ filter (`elem` valid) rgUp
    where
    rgUp = map Data.Char.toUpper $ T.unpack rg
    valid = ['0'..'9']++['A'..'Z']
    
filterAlphabet :: T.Text -> T.Text
filterAlphabet s = T.pack $ filter (`elem` valid) $ T.unpack s
    where
    valid = ' ' : ['a'..'z']++['A'..'Z']
 
    
rgCheck :: T.Text -> Bool
rgCheck rg = do
    if ((null rg_nb)||(length rg_nb > 14)) then
        False
    else
        True
    where
    rg_nb = T.unpack $ rgFormat rg
    
cpfCheck :: T.Text -> Bool
cpfCheck cpft = do 
    if (length cpf_nb == 11) then
        cpfVal
    else
        False
    where
    cpf_nb = T.unpack $ filterNumber cpft
    cpfInt = map digitToInt cpf_nb
    d1 = do
        let table1 = [10,9..2]
            table2 = [(cpfInt !! x)*(table1 !! x) | x <- [0..8]]
            sT2 = mod (sum table2) 11
        if (sT2 < 2) then 0
        else 11-sT2
    d2 = do
        let cpfD1 = (take 9 cpfInt) ++ [d1]
            table1 = [11,10..2]
            table2 = [(cpfD1 !! x)*(table1 !! x) | x <- [0..9]]
            sT2 = mod (sum table2) 11
        if (sT2 < 2) then 0
        else 11-sT2
    invCpf = [map (+x) [0,0,0,0,0,0,0,0,0,0,0] | x<-[0..9]]
    cpfVal = do
        if (elem cpfInt invCpf) then
            False
        else
            drop 9 cpfInt == [d1,d2]

paisCheck :: String -> Bool
paisCheck "BR" = True
paisCheck _ = False

cepCheck :: T.Text -> Bool
cepCheck x
    |length (T.unpack x) == 8 = True
    |otherwise = False
    
estadoCheck :: T.Text -> Bool
estadoCheck e = case estado of
    Just _ -> True
    Nothing -> False
    where
    estado = readMaybe $ T.unpack e :: Maybe Estado