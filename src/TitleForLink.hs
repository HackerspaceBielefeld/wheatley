{-# LANGUAGE OverloadedStrings #-}

module TitleForLink ( titleForLink ) where

import Network.HTTP.Conduit

import Data.Maybe
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Text.HTML.TagSoup
import qualified Text.Regex as R

isLink :: T.Text -> Bool
isLink t = isJust $ R.matchRegex urlRegex $ T.unpack t 

urlRegex :: R.Regex
urlRegex = R.mkRegex "((https?):((//)|(\\\\\\\\))+[\\w\\d:#@%/;$()~_?\\+-=\\\\\\.&]*)"

titleForLink :: T.Text -> IO (Maybe T.Text)
titleForLink "" = return Nothing
titleForLink t  = if isLink t then loadTitle t else return Nothing                              
                              where loadTitle :: T.Text -> IO (Maybe T.Text)
                                    loadTitle u = do
                                       request <- simpleHttp $ T.unpack u
                                       return $ extractTitle request

extractTitle :: B.ByteString ->  Maybe T.Text
extractTitle bs = do
   let tags = canonicalizeTags $ parseTags bs
   case takeWhile (not . isTagCloseName "title") $ dropWhile (not . isTagOpenName "title") tags of
        [] -> Nothing
        t  -> Just $ T.pack $ C.unpack $ foldl readTags "" t
          where readTags a (TagText b) = a `B.append` b
                readTags a _           = a
