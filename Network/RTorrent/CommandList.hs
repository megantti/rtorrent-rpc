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
