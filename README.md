rtorrent-rpc
============

This library can be used for communicating with RTorrent over its XML-RPC interface.

As an example, you can request torrent info and bandwidth usage:

```haskell
result <- callCommand "localhost" 5000 $ getAllTorrentInfo :*: getUpRate :*: getDownRate
case result of 
  Right (torrentInfo :*: uploadRate :*: downloadRate) -> ...
````
where

```
>>> :t torrentInfo
[TorrentInfo]
>>> :t uploadRate
Int
```

assuming you have set `scgi_port = localhost:5000` in your `.rtorrent.rc`.

As a more complete example, the following code finds all files that are over
100 megabytes, prints them along with the torrent they belong to and 
sets their priority to high.

```haskell
import Network.RTorrent

import Control.Monad
import Data.Monoid (mconcat)

main :: IO ()
main = do
    Right torrents <- callLocal . allTorrents $ 
                        getName <+> allFiles (getFilePath <+> getFileSizeBytes)
    let largeFiles = 
                filter (\(_ :*: _ :*: _ :*: size) -> size > 10^8)
                . concatMap (\(tName :*: fileList) -> 
                                map ((:*:) tName) fileList) 
                $ torrents
    putStrLn "Large files:"
    forM_ largeFiles $ \(torrent :*: _ :*: fPath :*: _) ->
        putStrLn $ "\t" ++ torrent ++ ": " ++ fPath
    _ <- callLocal 
        . MultiCommand
        . map (\(_ :*: fid :*: _ :*: _) -> 
              setFilePriority FilePriorityHigh fid)
          $ largeFiles
    return ()
```
