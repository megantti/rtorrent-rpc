{-# LANGUAGE TypeOperators, TypeFamilies, OverloadedStrings #-}

{-|
Module      : Peer
Copyright   : (c) Kai Lindholm, 2014, 2025
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

import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Text as T

import Network.RTorrent.Action.Internals
import Network.RTorrent.Torrent
import Network.RTorrent.Command
import Network.RTorrent.Value

data PeerId = PeerId !TorrentId !T.Text
    deriving Show

instance RpcType PeerId where
    toValue (PeerId (TorrentId tid) i) = ValueString $ tid <> ":p" <> i
    fromValue v = uncurry PeerId <$> (parse =<< fromValue v)
      where
        parse :: (Monad m, MonadFail m) => T.Text -> m (TorrentId, T.Text)
        parse str = do
            [hash, s] <- return $ T.splitOn ":p" str
            return (TorrentId hash, s)

instance NFData PeerId where
    rnf (PeerId tid i) = rnf tid `seq` rnf i

data PeerInfo = PeerInfo {
    peerClientVersion :: !T.Text
  , peerIp :: !T.Text
  , peerUpRate :: !Int
  , peerDownRate :: !Int
  , peerUpTotal :: !Int
  , peerDownTotal :: !Int
  , peerEncrypted :: !Bool
  , peerCompletedPercent :: !Int
  , peerPort :: !Int
  , peerId :: !PeerId
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

getPeerHash :: PeerId -> PeerAction T.Text
getPeerHash = simpleAction "p.id" []

getPeerIp :: PeerId -> PeerAction T.Text
getPeerIp = simpleAction "p.address" []

getPeerClientVersion :: PeerId -> PeerAction T.Text
getPeerClientVersion = simpleAction "p.client_version" []

getPeerUpRate :: PeerId -> PeerAction Int
getPeerUpRate = simpleAction "p.up_rate" []

getPeerDownRate :: PeerId -> PeerAction Int
getPeerDownRate = simpleAction "p.down_rate" []

getPeerUpTotal :: PeerId -> PeerAction Int
getPeerUpTotal = simpleAction "p.up_total" []

getPeerDownTotal :: PeerId -> PeerAction Int
getPeerDownTotal = simpleAction "p.down_total" []

getPeerEncrypted :: PeerId -> PeerAction Bool
getPeerEncrypted = fmap toEnum . simpleAction "p.is_encrypted" []

getPeerCompletedPercent :: PeerId -> PeerAction Int
getPeerCompletedPercent = simpleAction "p.completed_percent" []

getPeerPort :: PeerId -> PeerAction Int
getPeerPort = simpleAction "p.port" []

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

getTorrentPeers :: TorrentId -> TorrentAction (V.Vector PeerInfo)
getTorrentPeers = fmap (V.map contract) . allPeers getPeerPartial
  where
    contract (x :*: f) = f x

-- | Run the peer action on all peers that a torrent has.
allPeers :: (PeerId -> PeerAction a) -> TorrentId -> TorrentAction (V.Vector (PeerId :*: a))
allPeers p = fmap addId . (getTorrentId <+> allToMulti (allP (getPeerHash <+> p)))
  where
    addId (hash :*: peers) =
        V.map (\(phash :*: f) -> PeerId hash phash :*: f) peers
    allP :: (PeerId -> PeerAction a) -> AllAction PeerId a
    allP = AllAction (PeerId (TorrentId "") "") "p.multicall" (V.fromList [PString ""])
