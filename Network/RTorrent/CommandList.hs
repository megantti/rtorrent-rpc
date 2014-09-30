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
  ( getUpRate 
  , getDownRate
  , GetVar (..)
  , GetTorrentInfo (..)
  , module Network.RTorrent.Torrent
  , TorrentCommand 
  , AllTorrents (..)

  , start
  , close
  , setPriority
  )
  where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Error

import Control.DeepSeq

import Data.Monoid

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
    commandCall (GetVar cmd) = mkRTMethodCall cmd []
    commandValue _ = parseValue . single . single

-- | Torrent commands.
data TorrentCommand = TorrentCommand String [Param] TorrentId 

data Param = 
    PString String
  | PInt Int
  | PPriority Priority

instance Show Param where
    show (PString str) = show str
    show (PInt i) = show i
    show (PPriority p) = show (fromEnum p)

instance XmlRpcType Param where
    toValue (PString str) = toValue str
    toValue (PInt i) = toValue i
    toValue (PPriority p) = toValue p

    fromValue = fail "No fromValue for Params"
    getType _ = TUnknown

instance Command TorrentCommand where
    type Ret TorrentCommand = Value 
    commandCall (TorrentCommand command params tid) = 
        mkRTMethodCall command (toValue tid : map toValue params)

    commandValue _ = single

-- | Get the list of torrent infos.
data GetTorrentInfo = GetTorrentInfo

instance Command GetTorrentInfo where
    type Ret GetTorrentInfo = [TorrentInfo]
    commandCall _ = mkRTMethodCall "d.multicall" $ map ValueString [
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
start :: TorrentId -> TorrentCommand 
start = TorrentCommand "d.start" []

-- | Close a torrent.
close :: TorrentId -> TorrentCommand 
close = TorrentCommand "d.close" []

-- | Set the download priority of a torrent.
setPriority :: Priority -> TorrentId -> TorrentCommand 
setPriority pr = TorrentCommand "d.priority.set" [PPriority pr]

-- | Execute a command on all torrents.
-- For example 
--
-- > AllTorrents [close, setPriority PriorityNormal]
-- will close all torrents and set their priority to normal.
newtype AllTorrents = AllTorrents [TorrentId -> TorrentCommand] 

instance Monoid AllTorrents where
    mempty = AllTorrents []
    (AllTorrents as) `mappend` (AllTorrents bs) = AllTorrents (as ++ bs)
  
instance Command AllTorrents where
    type Ret AllTorrents = Value
    commandCall = mkRTMethodCall "d.multicall"
                    . map ValueString . ("" :) 
                    . map ((\(TorrentCommand cmd params _) -> cmd ++ "=" 
                                                                  ++ makeList params) 
                          . ($ emptyTid))
                    . get
      where
        emptyTid = TorrentId ""
        get (AllTorrents commands) = commands

        makeList :: Show a => [a] -> String
        makeList params = ('{' :) . go params $ "}"
          where
            go :: Show a => [a] -> ShowS
            go [x] = shows x 
            go (x:xs) = shows x . (',' :) . go xs
            go [] = id
    commandValue _ = single
