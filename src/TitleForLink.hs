{-# LANGUAGE OverloadedStrings #-}

module TitleForLink ( titleForLink ) where

import Network.HTTP
import Network.Browser
import Network.URI

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Text.HTML.TagSoup

titleForLink :: T.Text -> IO (Maybe T.Text)
titleForLink "" = return Nothing
titleForLink t  = case parseURI $ T.unpack t of
                       Nothing -> return Nothing
                       Just u  -> do
                          (_, Response _ _ _ res) <- browse $ bAction u
                          return $ extractTitle res 

extractTitle :: B.ByteString ->  Maybe T.Text
extractTitle bs = do
   let tags = canonicalizeTags $ parseTags bs
   case takeWhile (isTagCloseName "title") $ dropWhile (isTagOpenName "title") tags of
        [] -> Nothing
        t  -> Just $ T.pack $ C.unpack $ foldl readTags "" t
          where readTags a (TagText b) = a `B.append` b
                readTags a _           = a

bAction :: URI -> BrowserAction (HandleStream B.ByteString) (URI, Response B.ByteString)
bAction u = request $ mkRequest GET u
