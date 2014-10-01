{-# LANGUAGE TypeOperators, TypeFamilies #-}

{-|
Module      : File
Copyright   : (c) Kai Lindholm, 2014
License     : MIT
Maintainer  : megantti@gmail.com
Stability   : experimental
-}

module Network.RTorrent.File (
    FileId (..)
  , File (..)

  , getFilePath
  , getFrozenPath
  , getFileSizeBytes
  , getFileSizeChunks
  , getFileCompletedChunks
  , getFilePriority
  , setFilePriority
  , allFiles
  , getTorrentFiles
  , FileAction 
) where

import Control.DeepSeq

import Network.RTorrent.Action
import Network.RTorrent.Torrent
import Network.RTorrent.TorrentCommand
import Network.RTorrent.Commands
import Network.RTorrent.Priority
import Network.XmlRpc.Internals

import Data.List.Split (splitOn)

data FileId = FileId !TorrentId !Int deriving Show

instance XmlRpcType FileId where
    toValue (FileId tid i) = ValueString $ getTorrentId tid ++ ":f" ++ show i
    fromValue v = return . uncurry FileId . parse =<< fromValue v
      where
        parse :: String -> (TorrentId, Int)
        parse str = (TorrentId hash, read i)
          where
            [hash, i] = splitOn ":f" str

    getType _ = TString

data File = File {
    fileId :: FileId
  , filePath :: String
  , fileFrozenPath :: String
  , fileSizeBytes :: Int
  , fileSizeChunks :: Int
  , fileCompeletedChunks :: Int
  , filePriority :: FilePriority
} deriving Show

instance NFData FileId where
    rnf (FileId tid i) = rnf tid `seq` rnf i

instance NFData File where
    rnf (File fid fp ffp fsb fsc fcc fpt) = 
              rnf fid
        `seq` rnf fp 
        `seq` rnf ffp 
        `seq` rnf fsb 
        `seq` rnf fsc
        `seq` rnf fcc
        `seq` rnf fpt

type FileAction = Action FileId

-- | Run the file action on the all files that a torrent has.
allFiles :: (FileId -> FileAction a) -> (TorrentId -> TorrentAction [FileId :*: a])
allFiles f = fmap addId . (getHash <+> allToMulti (allF f))
  where
    addId (hash :*: files) = 
        zipWith (\index -> (:*:) (FileId hash index)) [0..] files 
    allF :: (FileId -> FileAction a) -> AllAction FileId a
    allF = AllAction (FileId (TorrentId "") 0) "f.multicall"

-- | Get the file name relative to the torrent base directory.
getFilePath :: FileId -> FileAction String
getFilePath = simpleAction "f.path" []

-- | Get the absolute path.
getFrozenPath :: FileId -> FileAction String
getFrozenPath = simpleAction "f.frozen_path" []

getFileSizeBytes :: FileId -> FileAction Int
getFileSizeBytes = simpleAction "f.size_bytes" []

getFileSizeChunks :: FileId -> FileAction Int
getFileSizeChunks = simpleAction "f.size_chunks" []

getFileCompletedChunks :: FileId -> FileAction Int
getFileCompletedChunks = simpleAction "f.completed_chunks" []

getFilePriority :: FileId -> FileAction FilePriority
getFilePriority = simpleAction "f.priority" []

setFilePriority :: FilePriority -> FileId -> FileAction Int
setFilePriority pr = simpleAction "f.priority.set" [PFilePriority pr]

getTorrentFiles :: TorrentId -> TorrentAction [File]
getTorrentFiles = (fmap . fmap . fmap) mkFile (allFiles action)
  where
    action = getFilePath
         <+> getFrozenPath
         <+> getFileSizeBytes
         <+> getFileSizeChunks
         <+> getFileCompletedChunks
         <+> getFilePriority
    mkFile   ( fid 
           :*: path
           :*: fpath
           :*: size
           :*: ch
           :*: cch
           :*: pr ) 
        = File fid path fpath size ch cch pr

