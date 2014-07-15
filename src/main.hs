{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Xmpp

import Data.Aeson
import Data.Text
import Data.Either
import qualified Data.ByteString.Lazy as B

import Control.Monad
import Control.Applicative
import Data.Default
import System.Log.Logger
--import GHC.Conc.Sync
import Control.Concurrent.MVar
import Control.Concurrent

main :: IO (MVar ())
main = do
   updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG
   conf <- readXmppConfig "wheatley.conf"
   case conf of
        Nothing -> do
           putStrLn "wheatley.conf couldn't been read."
           newEmptyMVar
        Just c  -> do 
           result <- session
              ( unpack $ server c ) 
              (Just (\_ -> [scramSha1 ( username c ) Nothing ( password c )] 
                 , Nothing))
              def
           sess <- case result of
              Right s -> return s
              Left e -> error $ "XmppFailure: " ++ show e
           sendPresence def sess
           do
              sessPresenceRequest <- dupSession sess
              mvarPresenceRequests <- newEmptyMVar
              forkFinally ( handlePresenceRequests sessPresenceRequest ) (\_ -> putMVar mvarPresenceRequests () )
              sessAnswerMessages <- dupSession sess
              mvarAnswerMessages <- newEmptyMVar
              forkFinally ( handleMessages sess ) (\_ -> putMVar mvarAnswerMessages () )
              return mvarPresenceRequests

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
        Left x  -> putStrLn $ show x
        Right x -> do
           let requestingJid = presenceFrom x
           case requestingJid of
                Nothing  -> putStrLn "No Jid to answer to found."
                Just jid -> do
                   let presenceAllowed = presenceSubscribed jid 
                   result <- sendPresence presenceAllowed sess
                   case result of
                     Left x  -> putStrLn $ show x
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
