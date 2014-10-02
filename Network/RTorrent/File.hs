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
  , FileAction 
  , getPartialFile
  , getTorrentFiles
  , allFiles

  -- * Functions dealing with a single variable
  , getFilePath
  , getAbsolutePath
  , getFileSizeBytes
  , getFileSizeChunks
  , getFileCompletedChunks
  , getFilePriority
  , setFilePriority
) where

import Control.Applicative
import Control.DeepSeq

import Network.RTorrent.Action
import Network.RTorrent.Torrent
import Network.RTorrent.Command
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
    filePath :: !String
  , fileSizeBytes :: !Int
  , fileSizeChunks :: !Int
  , fileCompeletedChunks :: !Int
  , filePriority :: !FilePriority
  , fileId :: !FileId
} deriving Show

instance NFData FileId where
    rnf (FileId tid i) = rnf tid `seq` rnf i

instance NFData File where
    rnf (File fid fp fsb fsc fcc fpt) = 
              rnf fp 
        `seq` rnf fsb 
        `seq` rnf fsc
        `seq` rnf fcc
        `seq` rnf fpt
        `seq` rnf fid

type FileAction = Action FileId

-- | Run the file action on all files that a torrent has.
allFiles :: (FileId -> FileAction a) -> TorrentId -> TorrentAction [FileId :*: a]
allFiles f = fmap addId . (getHash <+> allToMulti (allF f))
  where
    addId (hash :*: files) = 
        mapStrict id
        $ zipWith (\index -> (:*:) (FileId hash index)) [0..] files 
    allF :: (FileId -> FileAction a) -> AllAction FileId a
    allF = AllAction (FileId (TorrentId "") 0) "f.multicall"

-- | Get the file name relative to the torrent base directory.
getFilePath :: FileId -> FileAction String
getFilePath = simpleAction "f.path" []

-- | Get the absolute path.
getAbsolutePath :: FileId -> FileAction String
getAbsolutePath = simpleAction "f.frozen_path" []

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

-- | Get a file except for @FileId@. The @FileId@ can be got by running @allFiles@.
getPartialFile :: FileId -> FileAction (FileId -> File)
getPartialFile = runActionB $ File 
           <$> b getFilePath
           <*> b getFileSizeBytes
           <*> b getFileSizeChunks
           <*> b getFileCompletedChunks
           <*> b getFilePriority
  where
    b = ActionB

getTorrentFiles :: TorrentId -> TorrentAction [File]
getTorrentFiles = fmap (mapStrict contract) . allFiles getPartialFile
  where
    contract (x :*: f) = f x

