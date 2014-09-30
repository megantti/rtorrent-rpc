{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Commands
Copyright   : (c) Kai Lindholm, 2014
License     : MIT
Maintainer  : megantti@gmail.com
Stability   : experimental

A module for defined commands.
-}

module Network.RTorrent.CommandList 
  ( module Network.RTorrent.Torrent

  -- * Functions for getting global variables
  , GetVar 
  , getUpRate 
  , getDownRate
  , getSimple

  -- * Functions for handling torrents
  , TorrentAction
  , TorrentCommand
  , start
  , close
  , setPriority
  , getPriority
  , getHash 
  , getIsOpen
  , getTorrentUpRate
  , getTorrentDownRate
  , getSizeBytes
  , getLeftBytes
  , getName

  , getTorrentInfo

  , AllTorrents (..)
  , getAllTorrentInfo


  -- * Re-exported from "Network.RTorrent.Commands"
  , (<+>)
  , sequenceActions
  , (:*:)(..)
  , MultiCommand, mkMultiCommand
  )
  where

import Control.DeepSeq

import Network.XmlRpc.Internals

import Network.RTorrent.Commands
import Network.RTorrent.Torrent

bool :: Value -> Bool
bool (ValueInt 0) = False
bool (ValueInt 1) = True
bool (ValueBool b) = b
bool v = error $ "Failed to match a bool, got: " ++ show v


-- | Get a raw rtorrent variable.
getSimple :: (XmlRpcType a, NFData a) => String -> GetVar a
getSimple = GetVar parseSingle

-- | Get upload rate, in bytes per second.
getUpRate :: GetVar Int
getUpRate = getSimple "get_up_rate"

-- | Get download rate, in bytes per second.
getDownRate :: GetVar Int
getDownRate = getSimple "get_down_rate"

-- | Get a variable with result type @t@.
data GetVar t = GetVar (Value -> t) String
instance Command (GetVar a) where
    type Ret (GetVar a) = a
    commandCall (GetVar _ cmd) = mkRTMethodCall cmd []
    commandValue (GetVar parse _) = parse

instance Functor GetVar where
    fmap f (GetVar g s) = GetVar (f . g) s

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
         <+> getPriority
    mkInfo   ( hash 
           :*: name
           :*: open
           :*: down
           :*: up
           :*: size
           :*: left
           :*: pr ) 
        = TorrentInfo hash name open down up size left pr
    
parseSingle :: (XmlRpcType a, NFData a) => Value -> a
parseSingle = parseValue . single . single

simpleAction :: (XmlRpcType a, NFData a) => 
       String
    -> [Param] 
    -> TorrentId 
    -> TorrentAction a
simpleAction cmd params = TorrentAction [(cmd, params)] parseSingle

-- | Start downloading a torrent.
start :: TorrentId -> TorrentAction Int
start = simpleAction "d.start" []

-- | Close a torrent. 
close :: TorrentId -> TorrentAction Int
close = simpleAction "d.close" []

-- | Set the download priority of a torrent.
setPriority :: Priority -> TorrentId -> TorrentAction Int
setPriority pr = simpleAction "d.priority.set" [PPriority pr]

getHash :: TorrentId -> TorrentAction TorrentId
getHash = simpleAction "d.hash" []

getName :: TorrentId -> TorrentAction String
getName = simpleAction "d.get_name" []

getIsOpen :: TorrentId -> TorrentAction Bool
getIsOpen = TorrentAction [("d.is_open", [])] (bool . single . single)

getTorrentUpRate :: TorrentId -> TorrentAction Int
getTorrentUpRate = simpleAction "d.get_up_rate" []

getTorrentDownRate :: TorrentId -> TorrentAction Int
getTorrentDownRate = simpleAction "d.get_down_rate" []

getSizeBytes :: TorrentId -> TorrentAction Int
getSizeBytes = simpleAction "d.get_size_bytes" []

getLeftBytes :: TorrentId -> TorrentAction Int
getLeftBytes = simpleAction "d.get_left_bytes" []

getPriority :: TorrentId -> TorrentAction Priority
getPriority = simpleAction "d.priority" []

-- | Execute a command on all torrents.
-- For example 
--
-- > AllTorrents (setPriority PriorityNormal)
-- will will set the priority of all torrents to normal.
newtype AllTorrents a = AllTorrents (TorrentId -> TorrentAction a)

-- | A command that gets info for all torrents.
getAllTorrentInfo :: AllTorrents TorrentInfo
getAllTorrentInfo = AllTorrents getTorrentInfo
  
instance Command (AllTorrents a) where
    type Ret (AllTorrents a) = [a]
    commandCall (AllTorrents action) = 
                      mkRTMethodCall "d.multicall"
                    . map ValueString . ("" :) 
                    . map (\(cmd, params) -> cmd ++ "=" 
                                             ++ makeList params)
                    $ cmds
      where
        TorrentAction cmds _ _ = action emptyTid 
        emptyTid = TorrentId ""


        makeList :: Show a => [a] -> String
        makeList params = ('{' :) . go params $ "}"
          where
            go :: Show a => [a] -> ShowS
            go [x] = shows x 
            go (x:xs) = shows x . (',' :) . go xs
            go [] = id
    commandValue (AllTorrents action) = 
        map ( parse 
            . ValueArray 
            . map (ValueArray . (:[])) 
            . getArray) 
        . getArray . single . single
      where
        TorrentAction _ parse _ = action emptyTid
        emptyTid = TorrentId ""
