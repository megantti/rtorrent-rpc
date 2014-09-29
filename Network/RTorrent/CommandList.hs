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
  , TorrentInfo (..)
  )
  where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Error

import Network.XmlRpc.Internals

import Network.RTorrent.Commands

single :: Value -> Value
single (ValueArray [ar]) = ar
single v = error $ "Failed to match a singleton array, got: " ++ show v

int :: Value -> Int
int (ValueInt i) = i
int v = error $ "Failed to match a int, got: " ++ show v

bool :: Value -> Bool
bool (ValueInt 0) = False
bool (ValueInt 1) = True
bool (ValueBool b) = b
bool v = error $ "Failed to match a bool, got: " ++ show v

parseValue :: XmlRpcType a => Value -> a
parseValue = fromRight . runIdentity . runErrorT . fromValue 
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

data TorrentInfo = TorrentInfo {
      torrentId :: String
    , torrentName :: String
    , torrentOpen :: Bool
    , torrentDownRate :: Int
    , torrentUpRate :: Int
    , torrentSize :: Int
    , torrentBytesLeft :: Int
    } deriving Show

instance XmlRpcType TorrentInfo where
    toValue t = ValueArray [
            toValue $ torrentId t
          , toValue $ torrentName t
          , toValue $ torrentOpen t
          , toValue $ torrentDownRate t
          , toValue $ torrentUpRate t
          , toValue $ torrentSize t
          , toValue $ torrentBytesLeft t
        ]
    fromValue (ValueArray (v0:v1:v2:v3:v4:v5:v6:_)) = unwrapMonad $ TorrentInfo 
                  <$> from v0
                  <*> from v1
                  <*> pure (bool v2)
                  <*> from v3
                  <*> from v4
                  <*> from v5
                  <*> from v6
      where
        from :: (XmlRpcType a, Monad m) => Value -> WrappedMonad (ErrorT String m) a
        from = WrapMonad . fromValue
    fromValue _ = fail "TorrentInfo fromValue failed: expected a large enough valuearray"

    getType _ = TUnknown

-- | Get the list of torrent infos
data GetTorrentInfo = GetTorrentInfo

instance Command GetTorrentInfo where
    type Ret GetTorrentInfo = [TorrentInfo]
    commandCall _ = makeRTMethodCall "d.multicall" $ map ValueString [
                  ""
                , "d.local_id="
                , "d.get_name="
                , "d.is_open="
                , "d.get_down_rate="
                , "d.get_up_rate="
                , "d.get_size_bytes="
                , "d.get_left_bytes="
                ]
    commandValue _ = parseValue . single . single

