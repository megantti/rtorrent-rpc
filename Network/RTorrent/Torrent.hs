{-|
Module      : Torrent
Copyright   : (c) Kai Lindholm, 2014-2015
License     : MIT
Maintainer  : megantti@gmail.com
Stability   : experimental

For more info on actions, see "Network.RTorrent.Action".
-}

module Network.RTorrent.Torrent
  ( TorrentInfo (..)
  , TorrentId (..)
  , TorrentAction

  -- * Functions for handling torrents
  , start
  , close
  , stop
  , closeStop
  , erase
  , checkHash
  , getTorrent
  , getTorrents

  , allTorrents 

  -- * Functions for single variables
  , setTorrentPriority
  , getTorrentPriority
  , getTorrentId 
  , getTorrentOpen
  , getTorrentUpRate
  , getTorrentDownRate
  , getTorrentSizeBytes
  , getTorrentLeftBytes
  , getTorrentName
  , getTorrentPath
  , getTorrentDir
  , setTorrentDir
  , getTorrentRatio
  , getTorrentFileCount
  , getTorrentSizeChunks
  , getTorrentChunks
  , getTorrentChunkSize
  )
  where

import Control.Applicative
import Control.DeepSeq
import Network.XmlRpc.Internals

import Network.RTorrent.Action.Internals
import Network.RTorrent.Command.Internals
import Network.RTorrent.Chunk
import Network.RTorrent.Priority

-- | A newtype wrapper for torrent identifiers.
newtype TorrentId = TorrentId String 
    deriving Show

type TorrentAction = Action TorrentId

instance NFData TorrentId where
    rnf (TorrentId str) = rnf str

instance XmlRpcType TorrentId where
    toValue (TorrentId s) = ValueString s
    fromValue v = return . TorrentId =<< fromValue v
    getType _ = TString

data TorrentInfo = TorrentInfo {
      torrentId :: TorrentId
    , torrentName :: String
    , torrentOpen :: !Bool
    , torrentDownRate :: !Int
    , torrentUpRate :: !Int
    , torrentSize :: !Int
    , torrentBytesLeft :: !Int
    , torrentPath :: String
    , torrentDir :: String
    , torrentTorrentPriority :: !TorrentPriority
    } deriving Show

instance NFData TorrentInfo where
    rnf (TorrentInfo i a0 a1 a2 a3 a4 a5 a6 a7 a8) = 
        rnf i  `seq`
        rnf a0 `seq`
        rnf a1 `seq`
        rnf a2 `seq`
        rnf a3 `seq`
        rnf a4 `seq`
        rnf a5 `seq`
        rnf a6 `seq`
        rnf a7 `seq`
        rnf a8

-- | Get a TorrentInfo for a torrent.
getTorrent :: TorrentId -> TorrentAction TorrentInfo
getTorrent = runActionB $ TorrentInfo
         <$> b getTorrentId
         <*> b getTorrentName
         <*> b getTorrentOpen
         <*> b getTorrentDownRate
         <*> b getTorrentUpRate
         <*> b getTorrentSizeBytes
         <*> b getTorrentLeftBytes
         <*> b getTorrentPath
         <*> b getTorrentDir
         <*> b getTorrentPriority
  where
    b = ActionB
    
-- | Start downloading a torrent.
start :: TorrentId -> TorrentAction Int
start = simpleAction "d.start" []

-- | Close a torrent. 
close :: TorrentId -> TorrentAction Int
close = simpleAction "d.close" []

-- | Stop a torrent. 
stop :: TorrentId -> TorrentAction Int
stop = simpleAction "d.stop" []

closeStop :: TorrentId -> TorrentAction Int
closeStop = fmap (\(a :*: b) -> a + b) . (stop <+> close)

-- | Erase a torrent. 
erase :: TorrentId -> TorrentAction Int
erase = simpleAction "d.erase" []

-- | Initiate a hash check for a torrent.
checkHash :: TorrentId -> TorrentAction Int
checkHash = simpleAction "d.check_hash" []

-- | Set the download priority of a torrent.
setTorrentPriority :: TorrentPriority -> TorrentId -> TorrentAction Int
setTorrentPriority pr = simpleAction "d.priority.set" [PTorrentPriority pr]

getTorrentId :: TorrentId -> TorrentAction TorrentId
getTorrentId = simpleAction "d.hash" []

getTorrentName :: TorrentId -> TorrentAction String
getTorrentName = fmap decodeUtf8 . simpleAction "d.get_name" []

-- | Get the absolute path to the torrent's directory or file.
getTorrentPath :: TorrentId -> TorrentAction String
getTorrentPath = fmap decodeUtf8 . simpleAction "d.get_base_path" []

-- | Get the absolute path to the directory in which the torrent's directory or
-- file resides.
getTorrentDir :: TorrentId -> TorrentAction String
getTorrentDir = fmap decodeUtf8 . simpleAction "d.get_directory" []

setTorrentDir :: String -> TorrentId -> TorrentAction Int
setTorrentDir dir = simpleAction "d.set_directory" [PString dir]

getTorrentOpen :: TorrentId -> TorrentAction Bool
getTorrentOpen = fmap toEnum . simpleAction "d.is_open" []

getTorrentUpRate :: TorrentId -> TorrentAction Int
getTorrentUpRate = simpleAction "d.get_up_rate" []

getTorrentDownRate :: TorrentId -> TorrentAction Int
getTorrentDownRate = simpleAction "d.get_down_rate" []

getTorrentSizeBytes :: TorrentId -> TorrentAction Int
getTorrentSizeBytes = simpleAction "d.get_size_bytes" []

getTorrentLeftBytes :: TorrentId -> TorrentAction Int
getTorrentLeftBytes = simpleAction "d.get_left_bytes" []

getTorrentPriority :: TorrentId -> TorrentAction TorrentPriority
getTorrentPriority = simpleAction "d.priority" []

-- | Get the ratio (which is multiplied by a thousand)
getTorrentRatio :: TorrentId -> TorrentAction Int
getTorrentRatio = simpleAction "d.get_ratio" []

getTorrentFileCount :: TorrentId -> TorrentAction Int
getTorrentFileCount = simpleAction "d.get_size_files" []

-- | A total number of chunks.
getTorrentSizeChunks :: TorrentId -> TorrentAction Int
getTorrentSizeChunks = simpleAction "d.size_chunks" []

-- | Get a list that shows which chunks of the torrent are recorded as completed.
-- Will return 'Nothing' in the case that the torrent is closed.
getTorrentChunks :: TorrentId -> TorrentAction (Maybe [Bool])
getTorrentChunks = fmap combine . (getTorrentSizeChunks <+> chunks)
  where
    combine (count :*: ch) = convertChunksPad count ch
    chunks :: TorrentId -> TorrentAction String
    chunks = simpleAction "d.bitfield" []
    
-- | Get the size of a chunk.
getTorrentChunkSize :: TorrentId -> TorrentAction Int
getTorrentChunkSize = simpleAction "d.chunk_size" []

-- | Execute a command on all torrents.
-- For example the command
--
-- > allTorrents (setTorrentPriority TorrentPriorityNormal)
-- will set the priority of all torrents to normal.
allTorrents :: (TorrentId -> TorrentAction a) -> AllAction TorrentId a
allTorrents = AllAction (TorrentId "") "d.multicall"

-- | A command for getting torrent info for all torrents.
getTorrents :: AllAction TorrentId TorrentInfo
getTorrents = allTorrents getTorrent
