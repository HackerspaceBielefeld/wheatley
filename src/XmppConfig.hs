{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module XmppConfig (XmppConfig(..), readXmppConfig) where

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T

readXmppConfig :: String -> IO (Maybe XmppConfig)
readXmppConfig "" = return Nothing
readXmppConfig f  = do
   s <- B.readFile f  
   return ( decode s :: Maybe XmppConfig )

data XmppConfig = XmppConfig {
   server      :: T.Text,
   username    :: T.Text,
   password    :: T.Text,
   channel     :: T.Text,
   fingerprint :: T.Text}
   deriving (Show, Generic)

instance FromJSON XmppConfig
