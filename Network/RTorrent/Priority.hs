{-|
Module      : Priority
Copyright   : (c) Kai Lindholm, 2014, 2025
License     : MIT
Maintainer  : megantti@gmail.com
Stability   : experimental
-}

{-# LANGUAGE DeriveGeneric #-}

module Network.RTorrent.Priority (
    TorrentPriority (..)
  , FilePriority (..)
) where

import Control.DeepSeq
import GHC.Generics (Generic)
import Network.RTorrent.Value

data TorrentPriority =
      TorrentPriorityOff
    | TorrentPriorityLow
    | TorrentPriorityNormal
    | TorrentPriorityHigh
  deriving (Show, Eq, Ord, Generic)

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

instance RpcType TorrentPriority where
    toValue = toValue . fromEnum
    fromValue v = toEnum <$> (check =<< fromValue v)
      where
        check i
          | 0 <= i && i <= 3 = return i
          | otherwise = fail $ "Invalid TorrentPriority, got : " ++ show i

data FilePriority =
      FilePriorityOff
    | FilePriorityNormal
    | FilePriorityHigh
  deriving (Show, Eq, Ord, Generic)

instance NFData FilePriority

instance Enum FilePriority where
    toEnum 0 = FilePriorityOff
    toEnum 1 = FilePriorityNormal
    toEnum 2 = FilePriorityHigh
    toEnum i = error $ "toEnum :: Int -> FilePriority failed, got : " ++ show i

    fromEnum FilePriorityOff = 0
    fromEnum FilePriorityNormal = 1
    fromEnum FilePriorityHigh = 2

instance RpcType FilePriority where
    toValue = toValue . fromEnum
    fromValue v = toEnum <$> (check =<< fromValue v)
      where
        check i
          | 0 <= i && i <= 2 = return i
          | otherwise = fail $ "Invalid FilePriority, got : " ++ show i
