{-# LANGUAGE TypeOperators, TypeFamilies #-}

{-|
Module      : File
Copyright   : (c) Kai Lindholm, 2014
License     : MIT
Maintainer  : megantti@gmail.com
Stability   : experimental

For more info on actions, see "Network.RTorrent.Action".
-}

module Network.RTorrent.File (
    FileId (..)
  , FileInfo (..)
  , FileAction 
  , getFilePartial
  , getTorrentFiles
  , allFiles

  -- * Functions dealing with a single variable
  , getFilePath
  , getFileAbsolutePath
  , getFileSizeBytes
  , getFileSizeChunks
  , getFileCompletedChunks
  , getFilePriority
  , setFilePriority
  , getFileOffset
) where

import Control.Applicative
import Control.DeepSeq

import Network.RTorrent.Action.Internals
import Network.RTorrent.Torrent
import Network.RTorrent.Chunk
import Network.RTorrent.Command.Internals
import Network.RTorrent.Priority
import Network.XmlRpc.Internals

import Data.List.Split (splitOn)

data FileId = FileId !TorrentId !Int deriving Show

instance XmlRpcType FileId where
    toValue (FileId (TorrentId tid) i) = ValueString $ tid ++ ":f" ++ show i
    fromValue v = return . uncurry FileId =<< parse =<< fromValue v
      where
        parse :: Monad m => String -> m (TorrentId, Int)
        parse str = do
            [hash, i] <- return $ splitOn ":f" str
            return (TorrentId hash, read i)
    getType _ = TString

data FileInfo = FileInfo {
    filePath :: String
  , fileSizeBytes :: !Int
  , fileSizeChunks :: !Int
  , fileCompletedChunks :: !Int
  , filePriority :: !FilePriority
  , fileOffset :: !Int
  , fileId :: FileId
} deriving Show

instance NFData FileId where
    rnf (FileId tid i) = rnf tid `seq` rnf i

instance NFData FileInfo where
    rnf (FileInfo fid fp fsb fsc fcc fo fpt) = 
              rnf fp 
        `seq` rnf fsb 
        `seq` rnf fsc
        `seq` rnf fcc
        `seq` rnf fpt
        `seq` rnf fo
        `seq` rnf fid

type FileAction = Action FileId

-- | Run the file action on all files that a torrent has.
allFiles :: (FileId -> FileAction a) -> TorrentId -> TorrentAction [FileId :*: a]
allFiles f = fmap addId . (getTorrentId <+> allToMulti (allF f))
  where
    addId (hash :*: files) = 
        zipWith (\index -> (:*:) (FileId hash index)) [0..] files 
    allF :: (FileId -> FileAction a) -> AllAction FileId a
    allF = AllAction (FileId (TorrentId "") 0) "f.multicall"

-- | Get the file name relative to the torrent base directory.
getFilePath :: FileId -> FileAction String
getFilePath = fmap decodeUtf8 . simpleAction "f.path" []

-- | Get the absolute path.
getFileAbsolutePath :: FileId -> FileAction String
getFileAbsolutePath = fmap decodeUtf8 . simpleAction "f.frozen_path" []

getFileSizeBytes :: FileId -> FileAction Int
getFileSizeBytes = simpleAction "f.size_bytes" []

getFileSizeChunks :: FileId -> FileAction Int
getFileSizeChunks = simpleAction "f.size_chunks" []

getFileCompletedChunks :: FileId -> FileAction Int
getFileCompletedChunks = simpleAction "f.completed_chunks" []

-- | Get the offset of a file in a torrent,
-- when chunks are interpreted as continuous data.
getFileOffset :: FileId -> FileAction Int
getFileOffset = simpleAction "f.offset" []

getFilePriority :: FileId -> FileAction FilePriority
getFilePriority = simpleAction "f.priority" []

setFilePriority :: FilePriority -> FileId -> FileAction Int
setFilePriority pr = simpleAction "f.priority.set" [PFilePriority pr]

-- | Get a file except for @FileId@. The @FileId@ can be got by running @allFiles@.
getFilePartial :: FileId -> FileAction (FileId -> FileInfo)
getFilePartial = runActionB $ FileInfo
           <$> b getFilePath
           <*> b getFileSizeBytes
           <*> b getFileSizeChunks
           <*> b getFileCompletedChunks
           <*> b getFilePriority
           <*> b getFileOffset 
  where
    b = ActionB

getTorrentFiles :: TorrentId -> TorrentAction [FileInfo]
getTorrentFiles = fmap (map contract) . allFiles getFilePartial
  where
    contract (x :*: f) = f x

