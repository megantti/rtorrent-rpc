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

import System.IO ( IOMode (..) )

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8
import Blaze.Text
import Data.Attoparsec.ByteString.Lazy (Parser)
import qualified Data.Attoparsec.ByteString.Lazy as A
import Data.ByteString (ByteString)
import Data.Functor (($>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import Data.Char (ord)

import Network.Socket

type Headers = [(ByteString, ByteString)]

data Body = Body {
      headers :: Headers
    , body :: LB.ByteString
} deriving Show

makeRequest :: Body -> LB.ByteString
makeRequest (Body hd bd) = res
  where
    res = toLazyByteString . mconcat $ [
        integral len
      , fromChar ':'
      , fromByteString hdbs
      , fromChar ','
      , fromLazyByteString bd
      ]
    fromBS0 bs = fromWrite $ writeByteString bs <> writeWord8 0
    hdbs = toByteString . mconcat $
           (fromBS0 "CONTENT_LENGTH" <> integral (LB.length bd) <> fromWord8 0):
            map (\(a, b) -> fromBS0 a <> fromBS0 b) hd
    len = BS.length hdbs

parseResponse :: Parser Body
parseResponse = parseBody
  where
    lineChange :: A.Parser ByteString
    lineChange = A.string "\r\n"
    lineParser :: Parser ByteString
    lineParser =
        mappend <$> A.takeTill (== (fromIntegral $ ord '\r')) <*> ((lineChange $> "") <|> lineParser)
    headerParser = (,) <$> A.takeTill (==  (fromIntegral . ord $ ':')) <*> (A.string ": " *> lineParser)

    takeUntil :: Parser a -> Parser b -> Parser ([a], b)
    takeUntil a b = (end <$> b)
                    <|> (cont <$> a <*> takeUntil a b)
      where
        end x = ([], x)
        cont x (xs, y) = (x : xs, y)

    contentParser = lineChange  *> A.takeLazyByteString
    parseBody = do
        (headers, content) <- takeUntil headerParser contentParser
        return $ Body headers content

query :: HostName -> Int -> Body -> IO (Either String Body)
query host port queryBody = do
    let hints = defaultHints { addrSocketType = Stream, addrFamily = AF_INET }
    addr <- head <$> getAddrInfo (Just hints) (Just host) (Just $ show port)
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

    connect sock (addrAddress addr)
    h <- socketToHandle sock ReadWriteMode

    LB.hPut h (makeRequest queryBody)
    A.parseOnly parseResponse <$> LB.hGetContents h

