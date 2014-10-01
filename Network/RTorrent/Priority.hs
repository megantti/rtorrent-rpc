{-|
Module      : Priority
Copyright   : (c) Kai Lindholm, 2014
License     : MIT
Maintainer  : megantti@gmail.com
Stability   : experimental
-}

module Network.RTorrent.Priority (
    TorrentPriority (..)
  , FilePriority (..)
) where

import Control.DeepSeq
import Network.XmlRpc.Internals

data TorrentPriority = 
      TorrentPriorityOff
    | TorrentPriorityLow
    | TorrentPriorityNormal
    | TorrentPriorityHigh
  deriving (Show, Eq, Ord)

instance NFData TorrentPriority

instance Enum TorrentPriority where
    toEnum 0 = TorrentPriorityOff
    toEnum 1 = TorrentPriorityLow
    toEnum 2 = TorrentPriorityNormal
    toEnum 3 = TorrentPriorityHigh
    toEnum i = error $ "toEnum :: Int -> TorrentPriority failed, got : " ++ show i

    fromEnum TorrentPriorityOff = 0
    fromEnum TorrentPriorityLow = 1
    fromEnum TorrentPriorityNormal = 2
    fromEnum TorrentPriorityHigh = 3

instance XmlRpcType TorrentPriority where
    toValue = toValue . fromEnum
    fromValue v = return . toEnum =<< fromValue v
    getType _ = TInt

data FilePriority = 
      FilePriorityOff
    | FilePriorityNormal
    | FilePriorityHigh
  deriving (Show, Eq, Ord)

instance NFData FilePriority

instance Enum FilePriority where
    toEnum 0 = FilePriorityOff
    toEnum 1 = FilePriorityNormal
    toEnum 2 = FilePriorityHigh
    toEnum i = error $ "toEnum :: Int -> FilePriority failed, got : " ++ show i

    fromEnum FilePriorityOff = 0
    fromEnum FilePriorityNormal = 1
    fromEnum FilePriorityHigh = 2

instance XmlRpcType FilePriority where
    toValue = toValue . fromEnum
    fromValue v = return . toEnum =<< fromValue v
    getType _ = TInt
