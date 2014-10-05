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
    'getTorrents' ':*:' 'getUpRate' ':*:' 'getDownRate'
case result of 
  Right (torrentInfo :*: uploadRate :*: downloadRate) -> ...
@
where

>>> :t torrentInfo
[TorrentInfo]
>>> :t uploadRate
Int

assuming you have set @scgi_port = localhost:5000@ in your @.rtorrent.rc@.

Note that ':*:' is both a data constructor and a type constructor,
and therefore:

>>> :t True :*: False
Bool :*: Bool

However, using @:*:@ in types needs the @TypeOperators@ extension to work.


As a more complete example, the following code finds all files that are over
100 megabytes, prints them along with the torrent they belong to and 
sets their priorities to high.

@
import Control.Monad
import Network.RTorrent

main :: IO ()
main = do
    Right torrents <- callRTorrent "localhost" 5000 . 'allTorrents' $ 
                        'getTorrentName' '<+>' 'allFiles' ('getFilePath' '<+>' 'getFileSizeBytes')
    let largeFiles = 
                filter (\\(_ ':*:' _ ':*:' _ ':*:' size) -> size > 10^8)
                . concatMap (\\(tName :*: fileList) -> 
                                map ((:*:) tName) fileList) 
                $ torrents
    putStrLn "Large files:"
    forM_ largeFiles $ \\(torrent :*: _ :*: fPath :*: _) ->
        putStrLn $ "\\t" ++ torrent ++ ": " ++ fPath
    let cmd (_ :*: fid :*: _ :*: _) = 'setFilePriority' 'FilePriorityHigh' fid
    _ <- callRTorrent "localhost" 5000 $ map cmd largeFiles
    return ()
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
