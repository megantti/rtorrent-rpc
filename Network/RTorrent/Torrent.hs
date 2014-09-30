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
  , Priority (..)
  )
  where

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Error
import Network.XmlRpc.Internals

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
    , torrentPriority :: Priority
    } deriving Show

data Priority = 
      PriorityOff
    | PriorityLow
    | PriorityNormal
    | PriorityHigh
  deriving (Show, Eq, Ord)

instance NFData Priority

instance Enum Priority where
    toEnum 0 = PriorityOff
    toEnum 1 = PriorityLow
    toEnum 2 = PriorityNormal
    toEnum 3 = PriorityHigh
    toEnum i = error $ "toEnum :: Int -> Priority failed, got : " ++ show i

    fromEnum PriorityOff = 0
    fromEnum PriorityLow = 1
    fromEnum PriorityNormal = 2
    fromEnum PriorityHigh = 3

instance XmlRpcType Priority where
    toValue = toValue . fromEnum
    fromValue v = return . toEnum =<< fromValue v
    getType _ = TInt

instance NFData TorrentInfo where
    rnf (TorrentInfo i a0 a1 a2 a3 a4 a5 a6) = 
        rnf (getTorrentId i) `seq`
        rnf a0 `seq`
        rnf a1 `seq`
        rnf a2 `seq`
        rnf a3 `seq`
        rnf a4 `seq`
        rnf a5 `seq`
        rnf a6

