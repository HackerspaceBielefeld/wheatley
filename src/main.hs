{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Xmpp

import Data.Aeson
import Data.Text
import Data.Maybe
import qualified Data.ByteString.Lazy as B

import Control.Monad
import Control.Applicative
import System.Log.Logger
import Control.Concurrent

main :: IO ()
main = do
   updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG
   conf <- readXmppConfig "wheatley.conf"
   case conf of
        Nothing -> putStrLn "wheatley.conf couldn't be read."
        Just c  -> do 
           result <- session
              ( unpack $ server c ) 
              (Just ( const [scramSha1 ( username c ) Nothing ( password c )] 
                 , Nothing))
              def
           sess <- case result of
                        Right s -> return s
                        Left e  -> error $ "XmppFailure: " ++ show e
           _ <- sendPresence def sess
           let channelJid = fromMaybe ( error "Invalid Channel" ) ( jidFromText $ channel c ) 
           chan <- joinChannel sess channelJid 
           _ <- case chan of
                        Right s -> return s
                        Left e  -> error $ "XmppFailure: " ++ show e
           sessPresenceRequest <- dupSession sess
           _ <- forkIO $ handlePresenceRequests sessPresenceRequest
           sessAnswerMessages <- dupSession sess
           handleMessages sessAnswerMessages

joinChannel :: Session -> Jid -> IO ( Either XmppFailure () )
joinChannel sess chan = do
   let channelPresence = Presence Nothing Nothing ( Just chan ) Nothing Available []
   sendPresence channelPresence sess

handleMessages :: Session -> IO ()
handleMessages sess = forever $ do
   msg <- getMessage sess
   case answerMessage msg (messagePayload msg) of
        Just answer -> void $ sendMessage answer sess
        Nothing     -> putStrLn "Received message with no sender."

handlePresenceRequests :: Session -> IO ()
handlePresenceRequests sess = forever $ do
   presenceRequest <- pullPresence sess
   case presenceRequest of
        Left x  -> print x
        Right x -> do
           let requestingJid = presenceFrom x
           case requestingJid of
                Nothing  -> putStrLn "No Jid to answer to found."
                Just answerJid -> do
                   let presenceAllowed = presenceSubscribed answerJid 
                   result <- sendPresence presenceAllowed sess
                   case result of
                     Left y  -> print y
                     Right _ -> return ()

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
