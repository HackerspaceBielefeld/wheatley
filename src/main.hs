{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Xmpp
import Network.Xmpp.IM

import qualified Data.Text as T
import Data.Maybe

import Control.Monad
import Control.Applicative
import System.Log.Logger
import Control.Concurrent

import TitleForLink
import XmppConfig

main :: IO ()
main = do
   updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG
   conf <- readXmppConfig "wheatley.conf"
   case conf of
        Nothing -> putStrLn "wheatley.conf couldn't be read."
        Just c  -> do 
           result <- session
              ( T.unpack $ server c ) 
              (Just ( const [scramSha1 ( username c ) Nothing ( password c )] 
                 , Nothing))
              def
           sess <- case result of
                        Right s -> return s
                        Left e  -> error $ "XmppFailure: " ++ show e
           _ <- sendPresence def sess
           let channelJid = fromMaybe ( error "Invalid Channel" ) ( buildChannelJid c ) 
           chan <- joinChannel sess channelJid 
           _ <- case chan of
                        Right s -> return s
                        Left e  -> error $ "XmppFailure: " ++ show e
           sessPresenceRequest <- dupSession sess
           _ <- forkIO $ handlePresenceRequests sessPresenceRequest
           sessAnswerMessages <- dupSession sess
           handleMessages c sessAnswerMessages

buildChannelJid :: XmppConfig -> Maybe Jid
buildChannelJid x = jidFromText $ channel x `T.append` T.pack "/" `T.append` alias x

joinChannel :: Session -> Jid -> IO ( Either XmppFailure () )
joinChannel sess chan = do
   let channelPresence = Presence Nothing Nothing ( Just chan ) Nothing Available [] []
   sendPresence channelPresence sess

handleMessages :: XmppConfig ->  Session -> IO ()
handleMessages x sess = forever $ do
      let myJid = buildChannelJid x
      msg <- getMessage sess
      case (==) <$> myJid <*> messageFrom msg of
            Just True -> return ()
            _         -> case getIM msg of
                         Nothing                             -> return ()
                         Just (InstantMessage _ _ [])        -> return ()
                         Just (InstantMessage _ _ (MessageBody l c:_)) -> case T.words c of
                                               []         -> return ()   
                                               ":echo":cs -> do
                                                     let body       = MessageBody l $ T.unwords cs
                                                         ansMessage = answerMess msg body
                                                     case ansMessage of
                                                          Nothing -> return ()
                                                          Just m  -> do 
                                                             _ <- sendMessage m sess
                                                             return ()
                                               text       -> displayTitleForLinks sess msg text              

displayTitleForLinks :: Session -> Message -> [T.Text] -> IO ()
displayTitleForLinks sess msg text = do 
   titles <- mapM titleForLink text
   let links = zip titles text
   displayTitles links
     where displayTitles :: [(Maybe T.Text,  T.Text)] -> IO ()
           displayTitles []                     = return ()
           displayTitles ((Just title, orig):x) = do
              let body = MessageBody (messageLangTag msg) (T.strip title `T.append` T.pack "\n" `T.append` orig)
              case answerMess msg body of
                   Nothing -> return () 
                   Just m  -> do
                      _ <- sendMessage m sess
                      return ()
              displayTitles x
           displayTitles ((Nothing, _):x)       = displayTitles x

answerMess :: Message -> MessageBody -> Maybe Message
answerMess m b = case messageType m of
                      Chat      -> answerIM [b] m
                      GroupChat -> case messageFrom m of
                              Nothing -> Nothing
                              Just j  -> do
                                 let aMsg = Message Nothing (messageTo m) (Just $ toBare j) (messageLangTag m) GroupChat [] []
                                 Just $ withIM aMsg (InstantMessage Nothing [] [b])
                      _          -> Nothing

handlePresenceRequests :: Session -> IO ()
handlePresenceRequests sess = forever $ do
   presenceRequest <- pullPresence sess
   case presenceRequest of
        Left x  -> print x
        Right x -> case presenceFrom x of
                Nothing  -> putStrLn "No Jid to answer to found."
                Just answerJid -> do
                   let presenceAllowed = presenceSubscribed answerJid 
                   result <- sendPresence presenceAllowed sess
                   case result of
                     Left y  -> print y
                     Right _ -> return ()

      
