{-|
Module      : TorrentCommand
Copyright   : (c) Kai Lindholm, 2014
License     : MIT
Maintainer  : megantti@gmail.com
Stability   : experimental

-}

module Network.RTorrent.TorrentCommand (
    start
  , close
  , erase
  , getTorrentInfo
  , getAllTorrentInfo

  , allTorrents 

  -- * Functions for single variables
  , setTorrentPriority
  , getTorrentPriority
  , getHash 
  , getIsOpen
  , getTorrentUpRate
  , getTorrentDownRate
  , getSizeBytes
  , getLeftBytes
  , getName
  , getPath
  , getTorrentDir

) where

import Network.XmlRpc.Internals

import Network.RTorrent.Action
import Network.RTorrent.Commands
import Network.RTorrent.Torrent
import Network.RTorrent.Priority

bool :: Value -> Bool
bool (ValueInt 0) = False
bool (ValueInt 1) = True
bool (ValueBool b) = b
bool v = error $ "Failed to match a bool, got: " ++ show v

-- | Get a TorrentInfo for a torrent.
getTorrentInfo :: TorrentId -> TorrentAction TorrentInfo
getTorrentInfo = fmap (fmap mkInfo) action
  where
    action = getHash
         <+> getName
         <+> getIsOpen
         <+> getTorrentDownRate
         <+> getTorrentUpRate
         <+> getSizeBytes
         <+> getLeftBytes
         <+> getPath
         <+> getTorrentDir
         <+> getTorrentPriority
    mkInfo   ( hash 
           :*: name
           :*: open
           :*: down
           :*: up
           :*: size
           :*: left
           :*: pt
           :*: dir
           :*: pr ) 
        = TorrentInfo hash name open down up size left pt dir pr
    
-- | Start downloading a torrent.
start :: TorrentId -> TorrentAction Int
start = simpleAction "d.start" []

-- | Close a torrent. 
close :: TorrentId -> TorrentAction Int
close = simpleAction "d.close" []

-- | Erase a torrent. 
erase :: TorrentId -> TorrentAction Int
erase = simpleAction "d.erase" []

-- | Set the download priority of a torrent.
setTorrentPriority :: TorrentPriority -> TorrentId -> TorrentAction Int
setTorrentPriority pr = simpleAction "d.priority.set" [PTorrentPriority pr]

getHash :: TorrentId -> TorrentAction TorrentId
getHash = simpleAction "d.hash" []

getName :: TorrentId -> TorrentAction String
getName = simpleAction "d.get_name" []

getPath :: TorrentId -> TorrentAction String
getPath = simpleAction "d.get_base_path" []

getTorrentDir :: TorrentId -> TorrentAction String
getTorrentDir = simpleAction "d.get_directory" []

getIsOpen :: TorrentId -> TorrentAction Bool
getIsOpen = Action [("d.is_open", [])] (bool . single . single)

getTorrentUpRate :: TorrentId -> TorrentAction Int
getTorrentUpRate = simpleAction "d.get_up_rate" []

getTorrentDownRate :: TorrentId -> TorrentAction Int
getTorrentDownRate = simpleAction "d.get_down_rate" []

getSizeBytes :: TorrentId -> TorrentAction Int
getSizeBytes = simpleAction "d.get_size_bytes" []

getLeftBytes :: TorrentId -> TorrentAction Int
getLeftBytes = simpleAction "d.get_left_bytes" []

getTorrentPriority :: TorrentId -> TorrentAction TorrentPriority
getTorrentPriority = simpleAction "d.priority" []

-- | Execute a command on all torrents.
-- For example 
--
-- > allTorrents (setTorrentPriority TorrentPriorityNormal)
-- will will set the priority of all torrents to normal.
allTorrents :: (TorrentId -> TorrentAction a) -> AllAction TorrentId a
allTorrents = AllAction (TorrentId "") "d.multicall"

-- | A command for getting torrent info for all torrents.
getAllTorrentInfo :: AllAction TorrentId TorrentInfo
getAllTorrentInfo = allTorrents getTorrentInfo
