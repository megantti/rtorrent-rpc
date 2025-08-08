{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

{-|
Module      : RPC
Copyright   : (c) Kai Lindholm, 2014, 2025
License     : MIT
Maintainer  : megantti@gmail.com
Stability   : experimental

This package can be used for communicating with RTorrent over its JSON-RPC interface.

For example, you can request torrent info and bandwidth usage:

@
result <- callRTorrent "tcp://localhost:5000" $ 
    'getTorrents' 'Network.RTorrent.Command.:*:' 'getUpRate' 'Network.RTorrent.Command.:*:' 'getDownRate'
case result of 
  Right (torrentInfo :*: uploadRate :*: downloadRate) -> ...
@
where

>>> :t torrentInfo
Vector TorrentInfo
>>> :t uploadRate
Int

This requires you to have set @network.scgi.open_port = "127.0.0.1:5000"@ in your @.rtorrent.rc@,
but this comes with security implications if your computer has multiple users. 

Alternatively, you can setup RTorrent to open a UNIX socket with @network.scgi.open_local@ , and then call

@
result <- callRTorrent "unix:\/\/~/.rtorrent\/.session\/socket.rpc" $ 
    ...
@


Note that 'Network.RTorrent.Command.:*:' is both a data constructor and a type constructor,
and therefore:

>>> :t True :*: False
Bool :*: Bool

However, using @:*:@ in types needs the @TypeOperators@ extension to work.


As a more complete example, the following code finds all files that are over
100 megabytes, prints them along with the torrent they belong to and 
sets their priorities to high.

@
{&#45;\# LANGUAGE TypeOperators \#&#45;}

import Control.Monad
import Network.RTorrent
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T

-- This is an action, and they can be combined with ('<+>').
torrentInfo :: 'TorrentId'
                -> 'TorrentAction' (Text :*: Vector (FileId :*: Text :*: Int))
torrentInfo = 'getTorrentName' 
               '<+>' 'allFiles' ('getFilePath' <+> 'getFileSizeBytes')

-- 'allFiles' takes a file action ('FileId' -> 'FileAction' a)
-- and returns a torrent action: TorrentId -> 'TorrentAction' (Vector (FileId :*: a)).
-- Note that it automatically adds 'FileId's to all elements.

main :: IO ()
main = do
    Right torrents <- callRTorrent "tcp://localhost:5000" $
                        'allTorrents' torrentInfo
    let largeFiles = 
                V.filter (\\(_ :*: _ :*: _ :*: size) -> size > 10^8)
                . V.concatMap (\\(tName :*: fileList) -> 
                                V.map ((:*:) tName) fileList) 
                             -- Move the torrent name into the list
                $ torrents

    putStrLn "Large files:"
    V.forM_ largeFiles $ \\(torrent :*: _ :*: fPath :*: _) ->
        putStrLn $ "\\t" ++ T.unpack torrent ++ ": " ++ T.unpack fPath

    -- There is instance ('Network.RTorrent.Command.Command' cmdA, 'Network.RTorrent.Command.Command' cmdB) => 'Network.RTorrent.Command.Command' (cmdA :*: cmdB)
    -- The return value for the command cmdA is 'Network.RTorrent.Command.Ret' cmdA, which is an associated type
    -- in the Command type class.
    -- The return value for the command cmdA :*: cmdB is Ret cmdA :*: Ret cmdB.
                     
    let cmd :: Text :*: FileId :*: Text :*: Int 
                -> FileAction FilePriority :*: FileAction Int
        cmd (_ :*: fid :*: _ :*: _) = 
            'getFilePriority' fid :*: 'setFilePriority' 'FilePriorityHigh' fid

            -- Get the old priority and set the new one to high.
            -- setFilePriority returns a not-so-useful Int.

    -- There is also an instance Command a => Command (Vector a),
    -- and the return value for Vector a is Vector (Ret a).
    
    Right ret <- callRTorrent "tcp://localhost:5000" $ V.map cmd largeFiles

    putStrLn "Old priorities:"
    V.forM_ ret $ \\(oldPriority :*: _) -> do
        putStrLn $ "\\t" ++ show oldPriority
@
-}

module Network.RTorrent.RPC (
      module Network.RTorrent.CommandList
    , callRTorrent
    ) where

import Control.Monad.Except (ExceptT(..), throwError, runExceptT, liftEither)
import Control.Monad.IO.Class
import Control.Exception

import Data.Monoid
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB

import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Aeson as A
import Data.Aeson.Encode.Pretty

import Network.Socket
import Network.RTorrent.CommandList
import Network.RTorrent.Value
import Network.RTorrent.JSONRPC
import Network.RTorrent.Query
import qualified Network.RTorrent.Command.Internals as C

-- import Text.Show.Pretty (ppShow)

callRTorrentRaw :: String -> C.RTMethodCall -> ExceptT String IO Value
callRTorrentRaw addr calls = do
    let call = jsonRPCcall calls
    --liftIO . LB.putStr $ encodePretty call
    response <- ExceptT $ query addr call
    --response <- liftEither $ A.eitherDecode content
    --liftIO . LB.putStr $ encodePretty response
    --liftIO $ putStrLn "\njsonRPCdecode: "
    --liftIO . putStrLn . ppShow $ jsonRPCdecode response
    liftEither $ jsonRPCdecode response

-- | Call RTorrent with a command.
-- Only one connection is opened even when combining commands
-- for example by using ':*:' or lists.
--
-- The address can be currently be of the forms:
--
--     * tcp:\/\/localhost:5000                      
--     * unix:\/\/~\/.rtorrent\/.session\/rpc.socket    
callRTorrent :: Command a =>
    String   -- ^ Address
    -> a     -- ^ Command
    -> IO (Either String (Ret a))
callRTorrent addr command = do
    --print (C.commandCall command)
    runExceptT (do
        ret <- callRTorrentRaw addr (C.commandCall command)
        C.commandValue command ret)
        `catches` [ Handler (\e -> return . Left $ show (e :: IOException))
                  , Handler (\e -> return . Left $ show (e :: PatternMatchFail))]

