{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Xmpp

import Data.Aeson
import Data.Text
import qualified Data.ByteString.Lazy as B

import Control.Monad
import Control.Applicative
import Data.Default
import System.Log.Logger

main :: IO ()
main = do
   updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG
   result <- session
      "jabber.space.bi"
      (Just (\_ -> [scramSha1 "wheatley" Nothing "wheatley"]
         , Nothing))
      def
   sess <- case result of
      Right s -> return s
      Left e -> error $ "XmppFailure: " ++ show e
   sendPresence def sess
   forever $ do
      msg <- getMessage sess
      case answerMessage msg (messagePayload msg) of
         Just answer -> void $ sendMessage answer sess 
         Nothing -> putStrLn "Received message with no sender."

readXmppConfig :: String -> IO (Maybe XmppConfig)
readXmppConfig "" = return Nothing
readXmppConfig f  = do
   s <- B.readFile f  
   return ( decode s :: Maybe XmppConfig )

data XmppConfig = XmppConfig {
   server   :: Text,
   username :: Text,
   password :: Text,
   channel  :: Text}
   deriving Show

instance FromJSON XmppConfig where
   parseJSON (Object v) = XmppConfig      <$>
                          v .: "server"   <*>
                          v .: "username" <*>
                          v .: "password" <*>
                          v .: "channel"
   parseJSON _          = mzero
