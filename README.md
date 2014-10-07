rtorrent-rpc
============

This library can be used for communicating with RTorrent over its XML-RPC interface.

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

This requires you to have set `scgi_port = localhost:5000` in your `.rtorrent.rc`.

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
import Network.RTorrent

-- This is an action, and they can be combined with (<+>).
torrentInfo :: TorrentId
                -> TorrentAction (String :*: [FileId :*: String :*: Int])
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
                filter (\(_ :*: _ :*: _ :*: size) -> size > 10^8)
                . concatMap (\(tName :*: fileList) -> 
                                map ((:*:) tName) fileList) 
                             -- Move the torrent name into the list
                $ torrents

    putStrLn "Large files:"
    forM_ largeFiles $ \(torrent :*: _ :*: fPath :*: _) ->
        putStrLn $ "\t" ++ torrent ++ ": " ++ fPath

    -- There is instance (Command cmdA, Command cmdB) => Command (cmdA :*: cmdB)
    -- The return value for the command cmdA is Ret cmdA, which is an associated type
    -- in the Command type class.
    -- The return value for the command cmdA :*: cmdB is Ret cmdA :*: Ret cmdB.
                     
    let cmd :: String :*: FileId :*: String :*: Int 
                -> FileAction FilePriority :*: FileAction Int
        cmd (_ :*: fid :*: _ :*: _) = 
            getFilePriority fid :*: setFilePriority FilePriorityHigh fid

            -- Get the old priority and set the new one to high.
            -- setFilePriority returns a not-so-useful Int.

    -- There is also an instance Command a => Command [a],
    -- and the return value for [a] is [Ret a].
    
    Right ret <- callRTorrent "localhost" 5000 $ map cmd largeFiles

    putStrLn "Old priorities:"
    forM_ ret $ \(oldPriority :*: _) -> do
        putStrLn $ "\t" ++ show oldPriority
```
