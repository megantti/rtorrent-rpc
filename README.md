rtorrent-rpc
============

This library can be used for communicating with RTorrent over its JSON-RPC interface.

As an example, you can request torrent info and bandwidth usage:

```haskell
result <- callRTorrent "localhost" 5000 $ 
    getTorrents :*: getUpRate :*: getDownRate
case result of 
  Right (torrentInfo :*: uploadRate :*: downloadRate) -> ...
```
where

```
>>> :t torrentInfo
[TorrentInfo]
>>> :t uploadRate
Int
```

This requires you to have set @network.scgi.open_port = "127.0.0.1:5000"@ in your @.rtorrent.rc@,
but this comes with security implications if your computer has multiple users. 

Note that `:*:` is both a data constructor and a type constructor,
and therefore:

```
>>> :t True :*: False
Bool :*: Bool
```

However, using `:*:` in types needs the `TypeOperators` extension to work.


As a more complete example, the following code finds all files that are over
100 megabytes, prints them along with the torrent they belong to and 
sets their priorities to high.

```haskell
{-# LANGUAGE TypeOperators #-}

import Control.Monad

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T

import Network.RTorrent

-- This is an action, and they can be combined with (<+>).
torrentInfo :: TorrentId
                -> TorrentAction (Text :*: Vector (FileId :*: Text :*: Int))
torrentInfo = getTorrentName 
               <+> allFiles (getFilePath <+> getFileSizeBytes)

    -- allFiles takes a file action (FileId -> FileAction a)
    -- and returns a torrent action: TorrentId -> TorrentAction [FileId :*: a].
    -- Note that it automatically adds FileIds to all elements.

main :: IO ()
main = do
    Right torrents <- callRTorrent "localhost" 5000 $
                        allTorrents torrentInfo
    let largeFiles = 
                V.filter (\(_ :*: _ :*: _ :*: size) -> size > 10^8)
                . V.concatMap (\(tName :*: fileList) -> 
                                V.map ((:*:) tName) fileList) 
                             -- Move the torrent name into the list
                $ torrents

    putStrLn "Large files:"
    V.forM_ largeFiles $ \(torrent :*: _ :*: fPath :*: _) ->
        putStrLn $ "\t" ++ T.unpack torrent ++ ": " ++ T.unpack fPath

    -- There is instance ('Network.RTorrent.Command.Command' cmdA, 'Network.RTorrent.Command.Command' cmdB) => 'Network.RTorrent.Command.Command' (cmdA :*: cmdB)
    -- The return value for the command cmdA is 'Network.RTorrent.Command.Ret' cmdA, which is an associated type
    -- in the Command type class.
    -- The return value for the command cmdA :*: cmdB is Ret cmdA :*: Ret cmdB.
                     
    let cmd :: Text :*: FileId :*: Text :*: Int 
                -> FileAction FilePriority :*: FileAction Int
        cmd (_ :*: fid :*: _ :*: _) = 
            getFilePriority fid :*: setFilePriority FilePriorityHigh fid

            -- Get the old priority and set the new one to high.
            -- setFilePriority returns a not-so-useful Int.

    -- There is also an instance Command a => Command [a],
    -- and the return value for [a] is [Ret a].
    
    Right ret <- callRTorrent "localhost" 5000 $ V.map cmd largeFiles

    putStrLn "Old priorities:"
    V.forM_ ret $ \(oldPriority :*: _) -> do
        putStrLn $ "\t" ++ show oldPriority
```
