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
  , module Network.RTorrent.Priority
  , module Network.RTorrent.File
  , module Network.RTorrent.TorrentCommand

  -- * Functions for getting global variables
  , GetVar 
  , getUpRate 
  , getDownRate
  , getSimple
  , getDirectory
  , getPid

  , getUploadRate
  , getDownloadRate
  , setUploadRate
  , setDownloadRate

  , loadTorrent

  -- * Re-exported from "Network.RTorrent.Action"
  , (<+>)
  , sequenceActions
  , simpleAction
  -- * Re-exported from "Network.RTorrent.Commands"
  , (:*:)(..)
  , MultiCommand, mkMultiCommand
  )
  where

import Control.DeepSeq
import Network.XmlRpc.Internals

import Network.RTorrent.Action
import Network.RTorrent.Commands
import Network.RTorrent.File
import Network.RTorrent.Torrent
import Network.RTorrent.Priority
import Network.RTorrent.TorrentCommand


-- | Get a raw rtorrent variable.
getSimple :: (XmlRpcType a, NFData a) => String -> GetVar a
getSimple = GetVar parseSingle []

-- | Get the current up rate, in bytes per second.
getUpRate :: GetVar Int
getUpRate = getSimple "get_up_rate"

-- | Get the current down rate, in bytes per second.
getDownRate :: GetVar Int
getDownRate = getSimple "get_down_rate"

-- | Get the default download directory.
getDirectory :: GetVar String
getDirectory = getSimple "get_directory"

-- | Get the maximum upload rate, in bytes per second.
--
-- @0@ means no limit.
getUploadRate :: GetVar Int
getUploadRate = getSimple "get_upload_rate"

-- | Get the maximum download rate, in bytes per second.
--
-- @0@ means no limit.
getDownloadRate :: GetVar Int
getDownloadRate = getSimple "get_download_rate"

-- | Set the maximum upload rate, in bytes per second.
setUploadRate :: Int -> GetVar Int
setUploadRate i = GetVar parseSingle [ValueInt i] "set_upload_rate"

-- | Set the maximum download rate, in bytes per second.
setDownloadRate :: Int -> GetVar Int
setDownloadRate i = GetVar parseSingle [ValueInt i] "set_download_rate"

-- | Get the process id.
getPid :: GetVar Int
getPid = getSimple "system.pid"

-- | Load a torrent file.
loadTorrent :: String -- ^ Path
        -> GetVar Int
loadTorrent path = GetVar parseSingle [ValueString path] "load"

-- | Get a variable with result type @t@.
data GetVar t = GetVar (Value -> t) [Value] String
instance Command (GetVar a) where
    type Ret (GetVar a) = a
    commandCall (GetVar _ args cmd) = mkRTMethodCall cmd args
    commandValue (GetVar parse _ _) = parse

instance Functor GetVar where
    fmap f (GetVar g args s) = GetVar (f . g) args s
