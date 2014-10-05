{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : RPC
Copyright   : (c) Kai Lindholm, 2014
License     : MIT
Maintainer  : megantti@gmail.com
Stability   : experimental

This package can be used for communicating with RTorrent over its XML-RPC interface.

For example, you can request torrent info and bandwidth usage:

@
result <- callRTorrent "localhost" 5000 $ 
    'getTorrents' 'Network.RTorrent.Command.:*:' 'getUpRate' 'Network.RTorrent.Command.:*:' 'getDownRate'
case result of 
  Right (torrentInfo :*: uploadRate :*: downloadRate) -> ...
@
where

>>> :t torrentInfo
[TorrentInfo]
>>> :t uploadRate
Int

This requires you to have set @scgi_port = localhost:5000@ in your @.rtorrent.rc@.

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

-- This is an action, and they can be combined with ('<+>').
torrentInfo :: 'TorrentId'
                -> 'TorrentAction' (String :*: [FileId :*: String :*: Int])
torrentInfo = 'getTorrentName' 
               '<+>' 'allFiles' ('getFilePath' <+> 'getFileSizeBytes')

    -- 'allFiles' takes a file action ('FileId' -> 'FileAction' a)
    -- and returns a torrent action: TorrentId -> 'TorrentAction' [FileId :*: a].
    -- Note that it automatically adds 'FileId's to all elements.

main :: IO ()
main = do
    Right torrents <- callRTorrent "localhost" 5000 $
                        'allTorrents' torrentInfo
    let largeFiles = 
                filter (\\(_ :*: _ :*: _ :*: size) -> size > 10^8)
                . concatMap (\\(tName :*: fileList) -> 
                                map ((:*:) tName) fileList) 
                             -- Move the torrent name into the list
                $ torrents

    putStrLn "Large files:"
    forM_ largeFiles $ \\(torrent :*: _ :*: fPath :*: _) ->
        putStrLn $ "\\t" ++ torrent ++ ": " ++ fPath

    -- There is instance ('Network.RTorrent.Command.Command' a, 'Network.RTorrent.Command.Command' b) => 'Network.RTorrent.Command.Command' (a :*: b)
    -- The return value for the command a is 'Network.RTorrent.Command.Ret' a, which is an associated type
    -- in the Command type class.
    -- The return value for the command a :*: b is Ret a :*: Ret b.
                     
    let cmd :: String :*: FileId :*: String :*: Int 
                -> FileAction FilePriority :*: FileAction Int
        cmd (_ :*: fid :*: _ :*: _) = 
            'getFilePriority' fid :*: 'setFilePriority' 'FilePriorityHigh' fid

            -- Get the old priority and set the new one to high.
            -- setFilePriority returns a not-so-useful Int.

    -- There is also an instance Command a => Command [a],
    -- and the return value for [a] is [Ret a].
    
    Right ret <- callRTorrent "localhost" 5000 $ map cmd largeFiles

    putStrLn "Old priorities:"
    forM_ ret $ \\(oldPriority :*: _) -> do
        putStrLn $ "\\t" ++ show oldPriority
@
-}

module Network.RTorrent.RPC (
      module Network.RTorrent.CommandList
    , callRTorrent 
    ) where

import Control.Monad.Error (ErrorT(..), throwError, strMsg)
import Data.Monoid
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import Network
import Network.XmlRpc.Internals

import qualified Network.RTorrent.Command.Internals as C
import Network.RTorrent.CommandList
import Network.RTorrent.SCGI

callRTorrentRaw :: HostName -> Int -> C.RTMethodCall -> ErrorT String IO Value
callRTorrentRaw host port calls = do
    let request = Body [] . mconcat . LB.toChunks $ renderCall call 
    Body _ content <- ErrorT $ query host port request
    let cs = map (toEnum . fromEnum) $ BS.unpack content
    response <- parseResponse cs
    case response of
        Return ret -> case ret of
            ValueArray arr -> return $ ValueArray arr
            val -> throwError . strMsg $ "Got value of type " ++ show (getType val)
        Fault code err -> throwError . strMsg $ show code ++ ": " ++ err
  where
    call = MethodCall "system.multicall" [C.runRTMethodCall calls]

-- | Call RTorrent with a command.
-- Only one connection is opened even when combining commands
-- for example by using ':*:' or lists.
callRTorrent :: Command a =>
    HostName
    -> Int
    -> a 
    -> IO (Either String (Ret a))
callRTorrent host port command = 
    runErrorT $ 
        C.commandValue command =<< callRTorrentRaw host port (C.commandCall command)
