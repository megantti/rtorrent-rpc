{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Query
Copyright   : (c) Kai Lindholm, 2014, 2025
License     : MIT
Maintainer  : megantti@gmail.com
Stability   : experimental

An internal module for establishing a connection with RTorrent.
-}

module Network.RTorrent.Query (Headers, Body(..), query) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8
import Blaze.Text
import Control.Applicative
import Data.Char (ord)
import Data.Either (partitionEithers)
import Data.Functor (($>))
import Data.Monoid
import Network.Socket
import Network.URI (URI(..), URIAuth(..), parseURIReference)
import System.IO ( IOMode (..) )
import System.Directory (makeAbsolute, getHomeDirectory)

import qualified Data.Attoparsec.ByteString.Lazy as AT
import Data.Attoparsec.ByteString.Lazy (Parser)

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB

import Network.RTorrent.Value

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
    lineChange :: Parser ByteString
    lineChange = AT.string "\r\n"
    lineParser :: Parser ByteString
    lineParser =
        mappend <$> AT.takeTill (== (fromIntegral $ ord '\r')) <*> ((lineChange $> "") <|> lineParser)
    headerParser = (,) <$> AT.takeTill (==  (fromIntegral . ord $ ':')) <*> (AT.string ": " *> lineParser)

    takeUntil :: Parser a -> Parser b -> Parser ([a], b)
    takeUntil a b = (end <$> b)
                    <|> (cont <$> a <*> takeUntil a b)
      where
        end x = ([], x)
        cont x (xs, y) = (x : xs, y)

    contentParser = lineChange *> AT.takeLazyByteString
    parseBody = do
        (headers, content) <- takeUntil headerParser contentParser
        return $ Body headers content

querySCGI :: URI -> Body -> IO (Either String Body)
querySCGI uri queryBody = do
    let hints = if uriScheme uri == "tcp:"
        then defaultHints { addrSocketType = Stream, addrFamily = AF_INET }
        else defaultHints { addrSocketType = Stream, addrFamily = AF_UNIX }
    addr <- if uriScheme uri == "tcp:"
        then return $ maybe "" uriRegName (uriAuthority uri)
        else if (uriRegName <$> uriAuthority uri) == Just "~"
                then (++ uriPath uri) <$> getHomeDirectory 
                else return $ uriPath uri
    let port = if uriScheme uri == "tcp:"
        then drop 1 . uriPort <$> uriAuthority uri
        else Nothing
    addrl <- if uriScheme uri == "tcp:"
        then do
            getAddrInfo (Just hints) (Just addr) port
        else do
            return [AddrInfo [] AF_UNIX Stream 0 (SockAddrUnix addr) Nothing]
    case addrl of
        [] -> return (Left "Couldn't resolve SCGI address.")
        (addr:_) -> do
            sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            connect sock (addrAddress addr)
            h <- socketToHandle sock ReadWriteMode

            LB.hPut h (makeRequest queryBody)
            AT.parseOnly parseResponse <$> LB.hGetContents h

scgi :: URI -> Value -> IO (Either String Value)
scgi addr call = do
    let request = Body [] (A.encode call)
    resp <- querySCGI addr request
    return $ case resp of
        Left err -> Left err
        Right (Body _ content) -> A.eitherDecode content

-- | Send a query in @Value@ to the address and return the response or an error.
query :: String -> Value -> IO (Either String Value)
query addr call =
    case parseURIReference addr of
        Nothing -> return (Left "Invalid URI.")
        Just uri -> maybe (return $ Left "Unsupported URI protocol.")
                        (\f -> f uri call) $ lookup (uriScheme uri) schemes
    where
        schemes = [("tcp:", scgi), ("unix:", scgi)]
            --("http:", http), "https:", http)]
