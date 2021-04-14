{-# LANGUAGE TypeOperators, TypeFamilies, DeriveGeneric #-}

{-|
Module      : Tracker
Copyright   : (c) Kai Lindholm, 2014
License     : MIT
Maintainer  : megantti@gmail.com
Stability   : experimental

For more info on actions, see "Network.RTorrent.Action".
-}

module Network.RTorrent.Tracker (
    TrackerId (..)
  , TrackerType (..)
  , TrackerInfo (..)
  , TrackerAction 
  , getTrackerPartial
  , getTorrentTrackers
  , allTrackers

  -- * Functions dealing with a single variable
  , getTrackerUrl
  , getTrackerEnabled
  , setTrackerEnabled
  , getTrackerType
  , getTrackerOpen
) where

import Control.Applicative
import Control.DeepSeq

import GHC.Generics hiding ((:*:))

import Network.RTorrent.Action.Internals
import Network.RTorrent.Torrent
import Network.RTorrent.Command
import Network.XmlRpc.Internals

import Data.List.Split (splitOn)

data TrackerId = TrackerId !TorrentId !Int deriving Show

instance XmlRpcType TrackerId where
    toValue (TrackerId (TorrentId tid) i) = ValueString $ tid ++ ":t" ++ show i
    fromValue v = return . uncurry TrackerId =<< parse =<< fromValue v
      where
        parse :: MonadFail m => String -> m (TorrentId, Int)
        parse str = do
            [hash, i] <- return $ splitOn ":t" str
            return (TorrentId hash, read i)
    getType _ = TString

data TrackerType = 
      TrackerHTTP
    | TrackerUDP
    | TrackerDHT
  deriving (Show, Eq, Generic)

instance Enum TrackerType where
    toEnum 1 = TrackerHTTP
    toEnum 2 = TrackerUDP
    toEnum 3 = TrackerDHT
    toEnum i = error $ "toEnum :: Int -> TrackerType failed, got : " ++ show i

    fromEnum TrackerHTTP = 1
    fromEnum TrackerUDP = 2
    fromEnum TrackerDHT = 3

instance XmlRpcType TrackerType where
    toValue = toValue . fromEnum
    fromValue v = return . toEnum =<< check =<< fromValue v
      where
        check i 
          | 1 <= i && i <= 3 = return i
          | otherwise = fail $ "Invalid TrackerType, got : " ++ show i
    getType _ = TInt

instance NFData TrackerType

data TrackerInfo = TrackerInfo {
    trackerUrl :: String
  , trackerType :: !TrackerType
  , trackerEnabled :: !Bool
  , trackerOpen :: !Bool
  , trackerId :: TrackerId
} deriving Show

instance NFData TrackerId where
    rnf (TrackerId tid i) = rnf tid `seq` rnf i

instance NFData TrackerInfo where
    rnf (TrackerInfo a0 a1 a2 a3 a4) = 
              rnf a0 
        `seq` rnf a1
        `seq` rnf a2
        `seq` rnf a3
        `seq` rnf a4

type TrackerAction = Action TrackerId

-- | Run the tracker action on all trackers that a torrent has.
allTrackers :: (TrackerId -> TrackerAction a) -> TorrentId -> TorrentAction [TrackerId :*: a]
allTrackers t = fmap addId . (getTorrentId <+> allToMulti (allT t))
  where
    addId (hash :*: trackers) = 
        zipWith (\index -> (:*:) (TrackerId hash index)) [0..] trackers 
    allT :: (TrackerId -> TrackerAction a) -> AllAction TrackerId a
    allT = AllAction (TrackerId (TorrentId "") 0) "t.multicall"

getTrackerUrl :: TrackerId -> TrackerAction String
getTrackerUrl = simpleAction "t.get_url" []

getTrackerEnabled :: TrackerId -> TrackerAction Bool
getTrackerEnabled = fmap toEnum . simpleAction "t.is_enabled" []

setTrackerEnabled :: Bool -> TrackerId -> TrackerAction Int
setTrackerEnabled i = simpleAction "t.is_enabled" [PInt (fromEnum i)]

getTrackerType :: TrackerId -> TrackerAction TrackerType
getTrackerType = simpleAction "t.get_type" []

getTrackerOpen :: TrackerId -> TrackerAction Bool
getTrackerOpen = fmap toEnum . simpleAction "t.is_open" []

-- | Get a tracker except for @TrackerId@. The @TrackerId@ can be got by running @allTrackers@.
getTrackerPartial :: TrackerId -> TrackerAction (TrackerId -> TrackerInfo)
getTrackerPartial = runActionB $ TrackerInfo
           <$> b getTrackerUrl
           <*> b getTrackerType
           <*> b getTrackerEnabled
           <*> b getTrackerOpen
  where
    b = ActionB

getTorrentTrackers :: TorrentId -> TorrentAction [TrackerInfo]
getTorrentTrackers = fmap (map contract) . allTrackers getTrackerPartial
  where
    contract (x :*: f) = f x

