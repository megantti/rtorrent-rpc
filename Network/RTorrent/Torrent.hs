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
  , TorrentPriority (..)
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
    , torrentPriority :: TorrentPriority
    } deriving Show

data TorrentPriority = 
      PriorityOff
    | PriorityLow
    | PriorityNormal
    | PriorityHigh
  deriving (Show, Eq, Ord)

instance NFData TorrentPriority

instance Enum TorrentPriority where
    toEnum 0 = PriorityOff
    toEnum 1 = PriorityLow
    toEnum 2 = PriorityNormal
    toEnum 3 = PriorityHigh
    toEnum i = error $ "toEnum :: Int -> TorrentPriority failed, got : " ++ show i

    fromEnum PriorityOff = 0
    fromEnum PriorityLow = 1
    fromEnum PriorityNormal = 2
    fromEnum PriorityHigh = 3

instance XmlRpcType TorrentPriority where
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

bool :: Value -> Bool
bool (ValueInt 0) = False
bool (ValueInt 1) = True
bool (ValueBool b) = b
bool v = error $ "Failed to match a bool, got: " ++ show v

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
    fromValue (ValueArray (v0:v1:v2:v3:v4:v5:v6:v7:_)) = unwrapMonad $ TorrentInfo 
                  <$> from v0
                  <*> from v1
                  <*> pure (bool v2)
                  <*> from v3
                  <*> from v4
                  <*> from v5
                  <*> from v6
                  <*> from v7
      where
        from :: (XmlRpcType a, Monad m) => Value -> WrappedMonad (ErrorT String m) a
        from = WrapMonad . fromValue
    fromValue v = fail $ "TorrentInfo fromValue failed: expected a large enough valuearray" 
                        ++ ", got: " ++ show v

    getType _ = TUnknown
