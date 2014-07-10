{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Xmpp

import Control.Monad
import Data.Default
import System.Log.Logger

main :: IO ()
main = do
   updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG
   result <- session
      "jabber.space.bi"
      (Just (\_ -> ( [scramSha1 "wheatley" Nothing "wheatley"])
         , Nothing))
      def
   sess <- case result of
      Right s -> return s
      Left e -> error $ "XmppFailure: " ++ (show e)
   sendPresence def sess
   forever $ do
      msg <- getMessage sess
      case answerMessage msg (messagePayload msg) of
         Just answer -> sendMessage answer sess >> return ()
         Nothing -> putStrLn "Received message with no sender."
