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
  ( GetUpRate (..)
  , GetDownRate (..)
  , GetTorrentInfo (..)
  , module Network.RTorrent.Torrent
  , TorrentCommand (..)
  , start
  , close
  , AllTorrents (..)
  )
  where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Error

import Control.DeepSeq

import Network.XmlRpc.Internals

import Network.RTorrent.Commands
import Network.RTorrent.Torrent

single :: Value -> Value
single (ValueArray [ar]) = ar
single v = error $ "Failed to match a singleton array, got: " ++ show v

int :: Value -> Int
int (ValueInt i) = i
int v = error $ "Failed to match a int, got: " ++ show v

parseValue :: NFData a => XmlRpcType a => Value -> a
parseValue = force . fromRight . runIdentity . runErrorT . fromValue 
  where
    fromRight (Right r) = r
    fromRight (Left e) = error $ "parseValue failed: " ++ e
    

-- | Get up rate, in bytes
data GetUpRate = GetUpRate
instance Command GetUpRate where
    type Ret GetUpRate = Int
    commandCall _ = makeRTMethodCall "get_up_rate" []
    commandValue _= int . single . single

-- | Get down rate, in bytes
data GetDownRate = GetDownRate
instance Command GetDownRate where
    type Ret GetDownRate = Int
    commandCall _ = makeRTMethodCall "get_down_rate" []
    commandValue _ = int . single . single

-- | Get the list of torrent infos.
data GetTorrentInfo = GetTorrentInfo

instance Command GetTorrentInfo where
    type Ret GetTorrentInfo = [TorrentInfo]
    commandCall _ = makeRTMethodCall "d.multicall" $ map ValueString [
                  ""
                , "d.hash="
                , "d.get_name="
                , "d.is_open="
                , "d.get_down_rate="
                , "d.get_up_rate="
                , "d.get_size_bytes="
                , "d.get_left_bytes="
                , "d.priority="
                ]
    commandValue _ = parseValue . single . single

-- | Torrent commands
data TorrentCommand = TorrentCommand String TorrentId
instance Command TorrentCommand where
    type Ret TorrentCommand = () 
    commandCall (TorrentCommand command tid) = makeRTMethodCall command [toValue tid]
    commandValue _ _ = ()

-- | Start downloading a torrent.
start :: TorrentId -> TorrentCommand 
start = TorrentCommand "d.start"

-- | Close a torrent.
close :: TorrentId -> TorrentCommand 
close = TorrentCommand "d.close"

-- | Execute a command on all torrents.
-- For example 
--
-- > AllTorrents [close]
-- will close all torrents.
newtype AllTorrents = AllTorrents [TorrentId -> TorrentCommand] 
  
instance Command AllTorrents where
    type Ret AllTorrents = Value
    commandCall = makeRTMethodCall "d.multicall"
                    . map ValueString . ("" :) 
                    . map ((\(TorrentCommand cmd _) -> cmd ++ "=") . ($ emptyTid))
                    . get
      where
        emptyTid = TorrentId ""
        get (AllTorrents commands) = commands
    commandValue _ = single
