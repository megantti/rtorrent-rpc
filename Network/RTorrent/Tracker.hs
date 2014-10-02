{-# LANGUAGE TypeOperators, TypeFamilies #-}

{-|
Module      : Tracker
Copyright   : (c) Kai Lindholm, 2014
License     : MIT
Maintainer  : megantti@gmail.com
Stability   : experimental
-}

module Network.RTorrent.Tracker (
    TrackerId (..)
  , TrackerType (..)
  , Tracker (..)
  , TrackerAction 
  , getPartialTracker
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

import Network.RTorrent.Action
import Network.RTorrent.Torrent
import Network.RTorrent.Command
import Network.XmlRpc.Internals

import Data.List.Split (splitOn)

data TrackerId = TrackerId !TorrentId !Int deriving Show

instance XmlRpcType TrackerId where
    toValue (TrackerId tid i) = ValueString $ getTorrentId tid ++ ":t" ++ show i
    fromValue v = return . uncurry TrackerId . parse =<< fromValue v
      where
        parse :: String -> (TorrentId, Int)
        parse str = (TorrentId hash, read i)
          where
            [hash, i] = splitOn ":t" str

    getType _ = TString

data TrackerType = 
      TrackerHTTP
    | TrackerUDP
    | TrackerDHT
  deriving (Show, Eq)

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
    fromValue v = return . toEnum =<< fromValue v
    getType _ = TInt

instance NFData TrackerType

data Tracker = Tracker {
    trackerUrl :: !String
  , trackerType :: !TrackerType
  , trackerEnabled :: !Bool
  , trackerOpen :: !Bool
  , trackerId :: !TrackerId
} deriving Show

instance NFData TrackerId where
    rnf (TrackerId tid i) = rnf tid `seq` rnf i

instance NFData Tracker where
    rnf (Tracker a0 a1 a2 a3 a4) = 
              rnf a0 
        `seq` rnf a1
        `seq` rnf a2
        `seq` rnf a3
        `seq` rnf a4

type TrackerAction = Action TrackerId

-- | Run the tracker action on all trackers that a torrent has.
allTrackers :: (TrackerId -> TrackerAction a) -> TorrentId -> TorrentAction [TrackerId :*: a]
allTrackers t = fmap addId . (getHash <+> allToMulti (allT t))
  where
    addId (hash :*: trackers) = 
        forceFoldable
        $ zipWith (\index -> (:*:) (TrackerId hash index)) [0..] trackers 
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
getPartialTracker :: TrackerId -> TrackerAction (TrackerId -> Tracker)
getPartialTracker = runActionB $ Tracker 
           <$> b getTrackerUrl
           <*> b getTrackerType
           <*> b getTrackerEnabled
           <*> b getTrackerOpen
  where
    b = ActionB

getTorrentTrackers :: TorrentId -> TorrentAction [Tracker]
getTorrentTrackers = fmap (mapStrict contract) . allTrackers getPartialTracker
  where
    contract (x :*: f) = f x

