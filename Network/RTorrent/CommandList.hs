{-# LANGUAGE TypeFamilies, RankNTypes, OverloadedStrings #-}

{-|
Module      : Commands
Copyright   : (c) Kai Lindholm, 2014, 2025
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


import Control.Applicative

import Data.ByteString (ByteString)

import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Text as T

import Network.RTorrent.Action
import Network.RTorrent.Command.Internals
import Network.RTorrent.File
import Network.RTorrent.Peer
import Network.RTorrent.Priority
import Network.RTorrent.Torrent
import Network.RTorrent.Tracker
import Network.RTorrent.Value

-- | Run a command with no arguments.
commandSimple :: RpcType a => T.Text -> Global a
commandSimple cmd = commandArgs cmd []

-- | Run a command with the given arguments.
commandArgs :: RpcType a => T.Text -> [Value] -> Global a
commandArgs = flip $ Global parseSingle

-- | Run a command with the @Int@ given as an argument.
commandInt :: RpcType a => T.Text -> Int -> Global a
commandInt cmd i = commandArgs cmd [ValueInt i]

-- | Run a command with the @String@ given as an argument.
commandString :: RpcType a => 
    T.Text  -- ^ Command
    -> T.Text -- ^ Argument
    -> Global a
commandString cmd s = commandArgs cmd [ValueString s]

-- | Get the current up rate, in bytes per second.
getUpRate :: Global Int
getUpRate = commandSimple "throttle.global_up.rate"

-- | Get the current down rate, in bytes per second.
getDownRate :: Global Int
getDownRate = commandSimple "throttle.global_down.rate"

-- | Get the default download directory.
getDirectory :: Global T.Text
getDirectory = commandSimple "directory.default"

-- | Get the maximum upload rate, in bytes per second.
--
-- @0@ means no limit.
getUploadRate :: Global Int
getUploadRate = commandSimple "throttle.global_up.max_rate"

-- | Get the maximum download rate, in bytes per second.
--
-- @0@ means no limit.
getDownloadRate :: Global Int
getDownloadRate = commandSimple "throttle.global_down.max_rate"

-- | Set the maximum upload rate, in bytes per second.
setUploadRate :: Int -> Global Int
setUploadRate = commandInt "throttle.global_up.max_rate.set"

-- | Set the maximum download rate, in bytes per second.
setDownloadRate :: Int -> Global Int
setDownloadRate = commandInt "throttle.global_down.max_rate.set"

-- | Get the process id.
getPid :: Global Int
getPid = commandSimple "system.pid"

-- | Load a torrent file.
loadTorrent :: T.Text -- ^ A path / URL
        -> Global Int
loadTorrent = commandString "load"

-- | Load a torrent file.
loadTorrentRaw :: ByteString -- ^ A torrent file as data
        -> Global Int
--loadTorrentRaw torrentData = commandArgs "load_raw" [ValueBase64 torrentData] 
loadTorrentRaw torrentData = commandArgs "load.raw" [error "TODO"] 

-- | Load a torrent file and start downloading it.
loadStartTorrent :: T.Text -- ^ A path / URL
        -> Global Int
loadStartTorrent = commandString "load.start"

-- | Load a torrent file and start downloading it.
loadStartTorrentRaw :: ByteString -- ^ A torrent file as data
        -> Global Int
--loadStartTorrentRaw torrentData = commandArgs "load_raw_start" ["ValueBase64 torrentData] 
loadStartTorrentRaw torrentData = commandArgs "load.raw_start" [error "TODO"] 
    

-- | Execute a command with a result type @t@.
data Global t = Global (forall m. (Monad m, MonadFail m) => Value -> m t) [Value] T.Text
instance Command (Global a) where
    type Ret (Global a) = a
    commandCall (Global _ args cmd) = mkRTMethodCall cmd (V.fromList args)
    commandValue (Global parse _ _) = parse

instance Functor Global where
    fmap f (Global g args s) = Global (fmap f . g) args s

