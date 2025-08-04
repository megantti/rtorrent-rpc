{-# LANGUAGE TypeOperators, TypeFamilies, RankNTypes #-}

{-|
Module      : Action
Copyright   : (c) Kai Lindholm, 2014, 2025
License     : MIT
Maintainer  : megantti@gmail.com
Stability   : experimental

@Action@ is a command acting on various kinds of objects:

* torrents ('Network.RTorrent.Torrent.TorrentAction')
* files ('Network.RTorrent.File.FileAction')
* peers ('Network.RTorrent.Peer.PeerAction')
* trackers ('Network.RTorrent.Tracker.TrackerAction').

They all have the property that they can be executed on a single object or
on a group of objects.

For example, 

@
callRTorrent "localhost" 5000 $ some_action (some_id :: SomeId)
@
is a valid thing to write when @some@ is one of the previous objects.

To call an action on all torrents, you can use 'Network.RTorrent.Torrent.allTorrents', so that

@
callRTorrent "localhost" 5000 $ allTorrents getTorrentId
@
will return a list of torrent ids.

To call a action on other types of objects, you can use 'Network.RTorrent.Peer.allPeers',
'Network.RTorrent.File.allFiles', or 'Network.RTorrent.Tracker.allTrackers',
which will act on all peers, files, or trackers that are associated to a torrent.
They will also return ids for each object.
Then for example 

> allFiles getFileSizeBytes :: TorrentId -> TorrentAction [FileId :*: Int]
is an action that will return a list of ids and file sizes when run on a torrent.
These can further be used with 'Network.RTorrent.Torrent.allTorrents'.

To combine actions, you can use '<+>' and 'sequenceActions'
which correspond to ':*:' and @[]@ for commands.

In order to write new actions, 'simpleAction' can be used.

-}

module Network.RTorrent.Action (
      Action 
    , simpleAction
    , pureAction

    , sequenceActions
    , (<+>)

    , Param (..)
    , ActionB (..)

) where

import Network.RTorrent.Action.Internals
