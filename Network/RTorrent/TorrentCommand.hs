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
  , setTorrentPriority
  , getTorrentPriority
  , getHash 
  , getIsOpen
  , getTorrentUpRate
  , getTorrentDownRate
  , getSizeBytes
  , getLeftBytes
  , getName

  , getTorrentInfo

  , allTorrents 
  , getAllTorrentInfo
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

-- | Get the list of torrent infos.
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
         <+> getTorrentPriority
    mkInfo   ( hash 
           :*: name
           :*: open
           :*: down
           :*: up
           :*: size
           :*: left
           :*: pr ) 
        = TorrentInfo hash name open down up size left pr
    
-- | Start downloading a torrent.
start :: TorrentId -> TorrentAction Int
start = simpleAction "d.start" []

-- | Close a torrent. 
close :: TorrentId -> TorrentAction Int
close = simpleAction "d.close" []

-- | Set the download priority of a torrent.
setTorrentPriority :: TorrentPriority -> TorrentId -> TorrentAction Int
setTorrentPriority pr = simpleAction "d.priority.set" [PTorrentPriority pr]

getHash :: TorrentId -> TorrentAction TorrentId
getHash = simpleAction "d.hash" []

getName :: TorrentId -> TorrentAction String
getName = simpleAction "d.get_name" []

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

getAllTorrentInfo :: AllAction TorrentId TorrentInfo
getAllTorrentInfo = allTorrents getTorrentInfo
