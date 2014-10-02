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
  ( module Network.RTorrent.File
  , module Network.RTorrent.Peer
  , module Network.RTorrent.Priority
  , module Network.RTorrent.Torrent
  , module Network.RTorrent.Tracker

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
  , runInt
  , runString

  -- * Re-exported from "Network.RTorrent.Action"
  , (<+>)
  , sequenceActions
  , simpleAction
  -- * Re-exported from "Network.RTorrent.Command"
  , (:*:)(..)
  , AnyCommand (..)
  , Command (Ret)
  , mapStrict
  )
  where

import Network.XmlRpc.Internals

import Data.ByteString (ByteString)

import Network.RTorrent.Action
import Network.RTorrent.Command
import Network.RTorrent.File
import Network.RTorrent.Peer
import Network.RTorrent.Priority
import Network.RTorrent.Torrent
import Network.RTorrent.Tracker

-- | Run a command with no arguments.
runSimple :: XmlRpcType a => String -> Global a
runSimple cmd = runArgs cmd []

-- | Run a command with the given arguments.
runArgs :: XmlRpcType a => String -> [Value] -> Global a
runArgs = flip $ Global parseSingle

-- | Run a command with the @Int@ given as an argument.
runInt :: XmlRpcType a => String -> Int -> Global a
runInt cmd i = runArgs cmd [ValueInt i]

-- | Run a command with the @String@ given as an argument.
runString :: XmlRpcType a => 
    String  -- ^ Command
    -> String -- ^ Argument
    -> Global a
runString cmd s = runArgs cmd [ValueString s]

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
setUploadRate = runInt "set_upload_rate"

-- | Set the maximum download rate, in bytes per second.
setDownloadRate :: Int -> Global Int
setDownloadRate = runInt "set_download_rate"

-- | Get the process id.
getPid :: Global Int
getPid = runSimple "system.pid"

-- | Load a torrent file.
loadTorrent :: String -- ^ A path / URL
        -> Global Int
loadTorrent = runString "load"

-- | Load a torrent file.
loadTorrentRaw :: ByteString -- ^ A torrent file as data
        -> Global Int
loadTorrentRaw torrentData = runArgs "load_raw" [ValueBase64 torrentData] 

-- | Load a torrent file and start downloading it.
loadStartTorrent :: String -- ^ A path / URL
        -> Global Int
loadStartTorrent = runString "load_start"

-- | Load a torrent file and start downloading it.
loadStartTorrentRaw :: ByteString -- ^ A torrent file as data
        -> Global Int
loadStartTorrentRaw torrentData = runArgs "load_raw_start" [ValueBase64 torrentData] 
    

-- | Execute a command with a result type @t@.
data Global t = Global (Value -> t) [Value] String
instance Command (Global a) where
    type Ret (Global a) = a
    commandCall (Global _ args cmd) = mkRTMethodCall cmd args
    commandValue (Global parse _ _) = parse

instance Functor Global where
    fmap f (Global g args s) = Global (f . g) args s

