{-# LANGUAGE TypeOperators, TypeFamilies, OverloadedStrings #-}

{-|
Module      : Tracker
Copyright   : (c) Kai Lindholm, 2014, 2025
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

import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Text as T

import Network.RTorrent.Action.Internals
import Network.RTorrent.Torrent
import Network.RTorrent.Command
import Network.RTorrent.Value

-- import Data.List.Split (splitOn)

data TrackerId = TrackerId !TorrentId !Int deriving Show

instance RpcType TrackerId where
    toValue (TrackerId (TorrentId tid) i) = ValueString $ tid <> ":t" <> T.pack (show i)
    fromValue v = uncurry TrackerId <$> (parse =<< fromValue v)
      where
        parse :: (Monad m, MonadFail m) => T.Text -> m (TorrentId, Int)
        parse str = do
            [hash, i] <- return $ T.splitOn ":t" str
            return (TorrentId hash, read $ T.unpack i)

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

instance RpcType TrackerType where
    toValue = toValue . fromEnum
    fromValue v = toEnum <$> (check =<< fromValue v)
      where
        check i
          | 1 <= i && i <= 3 = return i
          | otherwise = fail $ "Invalid TrackerType, got : " ++ show i

data TrackerInfo = TrackerInfo {
    trackerUrl :: !T.Text
  , trackerType :: !TrackerType
  , trackerEnabled :: !Bool
  , trackerOpen :: !Bool
  , trackerId :: !TrackerId
} deriving Show

type TrackerAction = Action TrackerId

-- | Run the tracker action on all trackers that a torrent has.
allTrackers :: (TrackerId -> TrackerAction a) -> TorrentId -> TorrentAction (V.Vector (TrackerId :*: a))
allTrackers t = fmap addId . (getTorrentId <+> allToMulti (allT t))
  where
    addId (hash :*: trackers) =
        V.imap (\index -> (:*:) (TrackerId hash index)) trackers
    allT :: (TrackerId -> TrackerAction a) -> AllAction TrackerId a
    allT = AllAction (TrackerId (TorrentId "") 0) "t.multicall" (V.fromList [PString ""])

getTrackerUrl :: TrackerId -> TrackerAction T.Text
getTrackerUrl = simpleAction "t.url" []

getTrackerEnabled :: TrackerId -> TrackerAction Bool
getTrackerEnabled = fmap toEnum . simpleAction "t.is_enabled" []

setTrackerEnabled :: Bool -> TrackerId -> TrackerAction Int
setTrackerEnabled i = simpleAction "t.is_enabled.set" [PInt (fromEnum i)]

getTrackerType :: TrackerId -> TrackerAction TrackerType
getTrackerType = simpleAction "t.type" []

getTrackerOpen :: TrackerId -> TrackerAction Bool
getTrackerOpen = fmap toEnum . simpleAction "t.is_open" []

-- | Get @TrackerInfo@ for @TrackerId@. The @TrackerId@ can be got by running @allTrackers@.
getTrackerPartial :: TrackerId -> TrackerAction (TrackerId -> TrackerInfo)
getTrackerPartial = runActionB $ TrackerInfo
           <$> b getTrackerUrl
           <*> b getTrackerType
           <*> b getTrackerEnabled
           <*> b getTrackerOpen
  where
    b = ActionB

getTorrentTrackers :: TorrentId -> TorrentAction (V.Vector TrackerInfo)
getTorrentTrackers = fmap (V.map contract) . allTrackers getTrackerPartial
  where
    contract (x :*: f) = f x

