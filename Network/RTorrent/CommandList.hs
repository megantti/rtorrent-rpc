{-# LANGUAGE TypeFamilies, GADTs #-}

{-|
Module      : Commands
Copyright   : (c) Kai Lindholm, 2014
License     : MIT
Maintainer  : megantti@gmail.com
Stability   : experimental

A module for defined commands.
-}

module Network.RTorrent.CommandList 
  ( getUpRate 
  , getDownRate
  , GetVar (..)
  , GetTorrentInfo (..)
  , module Network.RTorrent.Torrent
  , TorrentCommand (..)
  , SingleParam, MultiParam
  , AllTorrents (..)
  , start
  , close
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

parseValue :: (NFData a, XmlRpcType a) => Value -> a
parseValue = force . fromRight . runIdentity . runErrorT . fromValue 
  where
    fromRight (Right r) = r
    fromRight (Left e) = error $ "parseValue failed: " ++ e
    

-- | Get upload rate, in bytes per second.
getUpRate :: GetVar Int
getUpRate = GetVar "get_up_rate"

-- | Get download rate, in bytes per second.
getDownRate :: GetVar Int
getDownRate = GetVar "get_down_rate"

-- | Get a variable with result type @t@.
data GetVar t = GetVar String
instance (NFData a, XmlRpcType a) => Command (GetVar a) where
    type Ret (GetVar a) = a
    commandCall (GetVar cmd) = makeRTMethodCall cmd []
    commandValue _ = parseValue . single . single

data SingleParam
data MultiParam

-- | Torrent commands
data TorrentCommand a where
    TorrentSCommand :: String -> TorrentId -> TorrentCommand SingleParam
    TorrentMCommand :: String -> [Value] -> TorrentId -> TorrentCommand MultiParam

instance Command (TorrentCommand a) where
    type Ret (TorrentCommand a) = Value 
    commandCall (TorrentSCommand command tid) = makeRTMethodCall command [toValue tid]
    commandCall (TorrentMCommand command params tid) = makeRTMethodCall command (toValue tid : params)

    commandValue _ = single


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

-- | Start downloading a torrent.
start :: TorrentId -> TorrentCommand SingleParam
start = TorrentSCommand "d.start"

-- | Close a torrent.
close :: TorrentId -> TorrentCommand SingleParam
close = TorrentSCommand "d.close" 

-- | Execute a command on all torrents.
-- For example 
--
-- > AllTorrents [close]
-- will close all torrents.
newtype AllTorrents = AllTorrents [TorrentId -> TorrentCommand SingleParam] 
  
instance Command AllTorrents where
    type Ret AllTorrents = Value
    commandCall = makeRTMethodCall "d.multicall"
                    . map ValueString . ("" :) 
                    . map ((\(TorrentSCommand cmd _) -> cmd ++ "=") . ($ emptyTid))
                    . get
      where
        emptyTid = TorrentId ""
        get (AllTorrents commands) = commands
    commandValue _ = single
