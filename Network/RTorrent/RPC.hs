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
    , callRTorrentEval
    ) where

import Control.Applicative
import Control.DeepSeq
import Control.Exception
import Control.Monad.Trans
import Control.Monad.Error (ErrorT(..), throwError, strMsg)
import Data.Monoid
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import Network
import Network.XmlRpc.Internals

import qualified Network.RTorrent.Command as C
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

callRTorrentEvalWith :: Command a =>
    (Ret a -> Ret a)
    -> HostName
    -> Int
    -> a 
    -> IO (Either String (Ret a))
callRTorrentEvalWith eval host port command = 
    handleException $ runErrorT . (>>= lift . evaluate . eval) $ 
        C.commandValue command <$> callRTorrentRaw host port (C.commandCall command)
  where
    handleException f = catch f (\e -> return . Left . show $ (e :: SomeException))

-- | Call RTorrent with a command.
-- Only one connection is opened even when using combinators
-- like ':*:' to combine commands.
callRTorrent :: Command a => 
    HostName -- ^ Hostname
    -> Int  -- ^ Port
    -> a -- ^ Command to send to RTorrent
    -> IO (Either String (Ret a))
callRTorrent = callRTorrentEvalWith id

-- | Like callRTorrent, but evaluates its result completely.
callRTorrentEval :: (Command a, NFData (Ret a)) => 
    HostName -- ^ Hostname
    -> Int  -- ^ Port
    -> a -- ^ Command to send to RTorrent
    -> IO (Either String (Ret a))
callRTorrentEval = callRTorrentEvalWith force

