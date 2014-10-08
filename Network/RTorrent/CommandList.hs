{-# LANGUAGE TypeFamilies, RankNTypes #-}

{-|
Module      : Commands
Copyright   : (c) Kai Lindholm, 2014
License     : MIT
Maintainer  : megantti@gmail.com
Stability   : experimental

A module for defined commands.

To combine multiple commands, use ':*:',
or store them in a list, 
as both of these types have 'Command' instances.

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

  , commandSimple
  , commandArgs
  , commandInt
  , commandString

  -- * Re-exported from "Network.RTorrent.Action"
  , (<+>)
  , sequenceActions

  -- * Re-exported from "Network.RTorrent.Command"
  , (:*:)(..)
  , AnyCommand (..)
  , Command (Ret)
  )
  where

import Network.XmlRpc.Internals

import Control.Applicative

import Data.ByteString (ByteString)

import Network.RTorrent.Action
import Network.RTorrent.Command.Internals
import Network.RTorrent.File
import Network.RTorrent.Peer
import Network.RTorrent.Priority
import Network.RTorrent.Torrent
import Network.RTorrent.Tracker

-- | Run a command with no arguments.
commandSimple :: XmlRpcType a => String -> Global a
commandSimple cmd = commandArgs cmd []

-- | Run a command with the given arguments.
commandArgs :: XmlRpcType a => String -> [Value] -> Global a
commandArgs = flip $ Global parseSingle

-- | Run a command with the @Int@ given as an argument.
commandInt :: XmlRpcType a => String -> Int -> Global a
commandInt cmd i = commandArgs cmd [ValueInt i]

-- | Run a command with the @String@ given as an argument.
commandString :: XmlRpcType a => 
    String  -- ^ Command
    -> String -- ^ Argument
    -> Global a
commandString cmd s = commandArgs cmd [ValueString s]

-- | Get the current up rate, in bytes per second.
getUpRate :: Global Int
getUpRate = commandSimple "get_up_rate"

-- | Get the current down rate, in bytes per second.
getDownRate :: Global Int
getDownRate = commandSimple "get_down_rate"

-- | Get the default download directory.
getDirectory :: Global String
getDirectory = fmap decodeUtf8 $ commandSimple "get_directory"

-- | Get the maximum upload rate, in bytes per second.
--
-- @0@ means no limit.
getUploadRate :: Global Int
getUploadRate = commandSimple "get_upload_rate"

-- | Get the maximum download rate, in bytes per second.
--
-- @0@ means no limit.
getDownloadRate :: Global Int
getDownloadRate = commandSimple "get_download_rate"

-- | Set the maximum upload rate, in bytes per second.
setUploadRate :: Int -> Global Int
setUploadRate = commandInt "set_upload_rate"

-- | Set the maximum download rate, in bytes per second.
setDownloadRate :: Int -> Global Int
setDownloadRate = commandInt "set_download_rate"

-- | Get the process id.
getPid :: Global Int
getPid = commandSimple "system.pid"

-- | Load a torrent file.
loadTorrent :: String -- ^ A path / URL
        -> Global Int
loadTorrent = commandString "load"

-- | Load a torrent file.
loadTorrentRaw :: ByteString -- ^ A torrent file as data
        -> Global Int
loadTorrentRaw torrentData = commandArgs "load_raw" [ValueBase64 torrentData] 

-- | Load a torrent file and start downloading it.
loadStartTorrent :: String -- ^ A path / URL
        -> Global Int
loadStartTorrent = commandString "load_start"

-- | Load a torrent file and start downloading it.
loadStartTorrentRaw :: ByteString -- ^ A torrent file as data
        -> Global Int
loadStartTorrentRaw torrentData = commandArgs "load_raw_start" [ValueBase64 torrentData] 
    

-- | Execute a command with a result type @t@.
data Global t = Global (forall m. (Monad m, Applicative m) => Value -> m t) [Value] String
instance Command (Global a) where
    type Ret (Global a) = a
    commandCall (Global _ args cmd) = mkRTMethodCall cmd args
    commandValue (Global parse _ _) = parse

instance Functor Global where
    fmap f (Global g args s) = Global (fmap f . g) args s

