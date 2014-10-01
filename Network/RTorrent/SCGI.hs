{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

{-|
Module      : SCGI
Copyright   : (c) Kai Lindholm, 2014
License     : MIT
Maintainer  : megantti@gmail.com
Stability   : experimental

An internal module for establishing a connection with RTorrent.
-}

module Network.RTorrent.SCGI (Headers, Body(..), query) where

import Control.Applicative
import Data.Either (partitionEithers)
import Data.Monoid

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8
import Blaze.Text
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Network

type Headers = [(ByteString, ByteString)]

data Body = Body {
      headers :: Headers
    , body :: ByteString
} deriving Show

makeRequest :: Body -> ByteString
makeRequest (Body hd bd) = res 
  where
    res = toByteString . mconcat $ [ 
        integral len
      , fromChar ':'
      , fromByteString hdbs
      , fromChar ','
      , fromByteString bd
      ]
    fromBS0 bs = fromWrite $ writeByteString bs <> writeWord8 0
    hdbs = toByteString . mconcat $ 
           (fromBS0 "CONTENT_LENGTH" <> integral (BS.length bd) <> fromWord8 0): 
            map (\(a, b) -> fromBS0 a <> fromBS0 b) hd
    len = BS.length hdbs

parseResponse:: Parser Body
parseResponse = parseBody
  where
    lineParser :: Parser ByteString
    lineParser = mappend <$> A.takeTill (== '\r') <*> (("\r\n" *> pure "") <|> lineParser)
    headerParser = (,) <$> A.takeTill (==  ':') <*> (": " *> lineParser) 

    contentParser = "\r\n" *> A.takeByteString
    parseBody = do
        header <- headerParser
        (moreHeaders, [content]) <- partitionEithers <$> A.many' (A.eitherP headerParser contentParser)
        return $ Body (header : moreHeaders) content

query :: HostName -> Int -> Body -> IO (Either String Body)
query host port queryBody = do
    h <- connectTo host (PortNumber (toEnum port))
    BS.hPut h (makeRequest queryBody) 
    A.parseOnly parseResponse <$> BS.hGetContents h

