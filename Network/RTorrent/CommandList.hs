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

  -- * Functions for global variables
  , Global 
  , getUpRate 
  , getDownRate
  , getDirectory
  , getPid

  , getUploadRate
  , getDownloadRate
  , setUploadRate
  , setDownloadRate
  
  -- * Loading new torrents

  , loadTorrent
  , loadTorrentRaw
  , loadStartTorrent
  , loadStartTorrentRaw

  -- * Constructing new commands

  , runSimple
  , runArgs

  -- * Re-exported from "Network.RTorrent.Action"
  , (<+>)
  , sequenceActions
  , simpleAction
  -- * Re-exported from "Network.RTorrent.Commands"
  , (:*:)(..)
  , AnyCommand (..)
  )
  where

import Control.DeepSeq
import Network.XmlRpc.Internals

import qualified Data.ByteString as BS
import Data.ByteString.Base64

import Network.RTorrent.Action
import Network.RTorrent.Commands
import Network.RTorrent.File
import Network.RTorrent.Torrent
import Network.RTorrent.Priority
import Network.RTorrent.TorrentCommand

-- | Run a command with no arguments.
runSimple :: (XmlRpcType a, NFData a) => String -> Global a
runSimple = runArgs []

-- | Run a command with the given arguments.
runArgs :: (XmlRpcType a, NFData a) => [Value] -> String -> Global a
runArgs = Global parseSingle

-- | Get the current up rate, in bytes per second.
getUpRate :: Global Int
getUpRate = runSimple "get_up_rate"

-- | Get the current down rate, in bytes per second.
getDownRate :: Global Int
getDownRate = runSimple "get_down_rate"

-- | Get the default download directory.
getDirectory :: Global String
getDirectory = runSimple "get_directory"

-- | Get the maximum upload rate, in bytes per second.
--
-- @0@ means no limit.
getUploadRate :: Global Int
getUploadRate = runSimple "get_upload_rate"

-- | Get the maximum download rate, in bytes per second.
--
-- @0@ means no limit.
getDownloadRate :: Global Int
getDownloadRate = runSimple "get_download_rate"

-- | Set the maximum upload rate, in bytes per second.
setUploadRate :: Int -> Global Int
setUploadRate i = Global parseSingle [ValueInt i] "set_upload_rate"

-- | Set the maximum download rate, in bytes per second.
setDownloadRate :: Int -> Global Int
setDownloadRate i = Global parseSingle [ValueInt i] "set_download_rate"

-- | Get the process id.
getPid :: Global Int
getPid = runSimple "system.pid"

-- | Load a torrent file.
loadTorrent :: String -- ^ A path / URL
        -> Global Int
loadTorrent path = Global parseSingle [ValueString path] "load"

-- | Load a torrent file.
loadTorrentRaw :: BS.ByteString -- ^ A torrent file as data
        -> Global Int
loadTorrentRaw torrentData = Global parseSingle [ValueBase64 torrentData] "load_raw"

-- | Load a torrent file and start downloading it.
loadStartTorrent :: String -- ^ A path / URL
        -> Global Int
loadStartTorrent path = Global parseSingle [ValueString path] "load_start"

-- | Load a torrent file and start downloading it.
loadStartTorrentRaw :: BS.ByteString -- ^ A torrent file as data
        -> Global Int
loadStartTorrentRaw torrentData = Global parseSingle [ValueBase64 torrentData] "load_raw_start"
    

-- | Execute a command with a result type @t@.
data Global t = Global (Value -> t) [Value] String
instance Command (Global a) where
    type Ret (Global a) = a
    commandCall (Global _ args cmd) = mkRTMethodCall cmd args
    commandValue (Global parse _ _) = parse

instance Functor Global where
    fmap f (Global g args s) = Global (f . g) args s

