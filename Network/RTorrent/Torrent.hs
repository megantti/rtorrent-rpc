{-|
Module      : Torrent
Copyright   : (c) Kai Lindholm, 2014
License     : MIT
Maintainer  : megantti@gmail.com
Stability   : experimental

-}

module Network.RTorrent.Torrent
  ( TorrentInfo (..)
  , TorrentId (..)
  )
  where

import Control.DeepSeq
import Network.XmlRpc.Internals

import Network.RTorrent.Priority

-- | A newtype wrapper for torrent identifiers.
newtype TorrentId = TorrentId { 
      getTorrentId :: String 
    } deriving Show

instance NFData TorrentId where
    rnf (TorrentId str) = rnf str

instance XmlRpcType TorrentId where
    toValue = ValueString . getTorrentId
    fromValue v = return . TorrentId =<< fromValue v
    getType _ = TString

data TorrentInfo = TorrentInfo {
      torrentId :: TorrentId
    , torrentName :: String
    , torrentOpen :: Bool
    , torrentDownRate :: Int
    , torrentUpRate :: Int
    , torrentSize :: Int
    , torrentBytesLeft :: Int
    , torrentPath :: String
    , torrentDir :: String
    , torrentTorrentPriority :: TorrentPriority
    } deriving Show

instance NFData TorrentInfo where
    rnf (TorrentInfo i a0 a1 a2 a3 a4 a5 a6 a7 a8) = 
        rnf i  `seq`
        rnf a0 `seq`
        rnf a1 `seq`
        rnf a2 `seq`
        rnf a3 `seq`
        rnf a4 `seq`
        rnf a5 `seq`
        rnf a6 `seq`
        rnf a7 `seq`
        rnf a8

