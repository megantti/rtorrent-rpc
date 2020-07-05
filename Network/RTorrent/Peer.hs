{-# LANGUAGE TypeOperators, TypeFamilies #-}

{-|
Module      : Peer
Copyright   : (c) Kai Lindholm, 2014
License     : MIT
Maintainer  : megantti@gmail.com
Stability   : experimental

For more info on actions, see "Network.RTorrent.Action".
-}

module Network.RTorrent.Peer (
    PeerId (..)
  , PeerInfo (..)
  , PeerAction 

  , getPeerPartial
  , allPeers
  , getTorrentPeers

  -- * Control peers
  , banPeer
  , disconnectPeer

  -- * Functions for single variables
  , getPeerHash
  , getPeerIp

  , getPeerClientVersion
  , getPeerUpRate
  , getPeerDownRate
  , getPeerUpTotal
  , getPeerDownTotal
  , getPeerEncrypted
  , getPeerCompletedPercent
  , getPeerPort
) where

import Control.Applicative
import Control.DeepSeq

import Network.RTorrent.Action.Internals
import Network.RTorrent.Torrent
import Network.RTorrent.Command
import Network.XmlRpc.Internals

import Data.List.Split (splitOn)

data PeerId = PeerId !TorrentId !String 
    deriving Show

instance XmlRpcType PeerId where
    toValue (PeerId (TorrentId tid) i) = ValueString $ tid ++ ":p" ++ i
    fromValue v = return . uncurry PeerId =<< parse =<< fromValue v
      where
        parse :: MonadFail m => String -> m (TorrentId, String)
        parse str = do
            [hash, s] <- return $ splitOn ":p" str
            return (TorrentId hash, s)
    getType _ = TString

instance NFData PeerId where
    rnf (PeerId tid i) = rnf tid `seq` rnf i

data PeerInfo = PeerInfo {
    peerClientVersion :: String
  , peerIp :: String
  , peerUpRate :: !Int
  , peerDownRate :: !Int
  , peerUpTotal :: !Int
  , peerDownTotal :: !Int
  , peerEncrypted :: !Bool
  , peerCompletedPercent :: !Int
  , peerPort :: !Int
  , peerId :: PeerId
} deriving Show

instance NFData PeerInfo where
    rnf (PeerInfo a0 a1 a2 a3 a4 a5 a6 a7 a8 a9) =
              rnf a0
        `seq` rnf a1 
        `seq` rnf a2
        `seq` rnf a3
        `seq` rnf a4
        `seq` rnf a5
        `seq` rnf a6
        `seq` rnf a7
        `seq` rnf a8
        `seq` rnf a9

getPeerHash :: PeerId -> PeerAction String
getPeerHash = simpleAction "p.get_id" []

getPeerIp :: PeerId -> PeerAction String
getPeerIp = simpleAction "p.get_address" []

getPeerClientVersion :: PeerId -> PeerAction String
getPeerClientVersion = simpleAction "p.get_client_version" []

getPeerUpRate :: PeerId -> PeerAction Int
getPeerUpRate = simpleAction "p.get_up_rate" []

getPeerDownRate :: PeerId -> PeerAction Int
getPeerDownRate = simpleAction "p.get_down_rate" []

getPeerUpTotal :: PeerId -> PeerAction Int
getPeerUpTotal = simpleAction "p.get_up_total" []

getPeerDownTotal :: PeerId -> PeerAction Int
getPeerDownTotal = simpleAction "p.get_down_total" []

getPeerEncrypted :: PeerId -> PeerAction Bool
getPeerEncrypted = fmap toEnum . simpleAction "p.is_encrypted" []

getPeerCompletedPercent :: PeerId -> PeerAction Int
getPeerCompletedPercent = simpleAction "p.get_completed_percent" []

getPeerPort :: PeerId -> PeerAction Int
getPeerPort = simpleAction "p.get_port" []

-- | Get a partial peer. @PeerId@ can be gotten by running @allPeers@.
getPeerPartial :: PeerId -> PeerAction (PeerId -> PeerInfo)
getPeerPartial = runActionB $ PeerInfo
         <$> b getPeerClientVersion
         <*> b getPeerIp
         <*> b getPeerUpRate
         <*> b getPeerDownRate
         <*> b getPeerUpTotal
         <*> b getPeerDownTotal
         <*> b getPeerEncrypted
         <*> b getPeerCompletedPercent
         <*> b getPeerPort
  where
    b = ActionB

disconnectPeer :: PeerId -> PeerAction Int
disconnectPeer = simpleAction "p.disconnect" []

banPeer :: PeerId -> PeerAction Int
banPeer = simpleAction "p.banned.set" [PInt 1]

type PeerAction = Action PeerId

getTorrentPeers :: TorrentId -> TorrentAction [PeerInfo]
getTorrentPeers = fmap (map contract) . allPeers getPeerPartial
  where
    contract (x :*: f) = f x

-- | Run the peer action on all peers that a torrent has.
allPeers :: (PeerId -> PeerAction a) -> TorrentId -> TorrentAction [PeerId :*: a]
allPeers p = fmap addId . (getTorrentId <+> allToMulti (allP (getPeerHash <+> p)))
  where
    addId (hash :*: peers) = 
        map (\(phash :*: f) -> PeerId hash phash :*: f) peers
    allP :: (PeerId -> PeerAction a) -> AllAction PeerId a
    allP = AllAction (PeerId (TorrentId "") "") "p.multicall"
