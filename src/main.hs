{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Xmpp

import Control.Monad
import Data.Default
import System.Log.Logger

--updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG

con = do
            result <- session
                     "jabber.space.bi"
                     (Just (\_ -> ( [scramSha1 "wheatley" Nothing "wheatley"] )
                           , Nothing))
                     def
            sess <- case result of
                         Right s -> return s
                         Left e  -> error $ "XmppFailure: " ++ show(e)
            return sess
