{-# LANGUAGE TypeOperators, TypeFamilies, OverloadedStrings #-}

{-|
Module      : File
Copyright   : (c) Kai Lindholm, 2014, 2025
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
  , getTorrentFileInfo
  , getTorrentFileInfoFiltered
  , allFiles
  , allFilesFiltered

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

import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Text as T

import Network.RTorrent.Action.Internals
import Network.RTorrent.Torrent
import Network.RTorrent.Chunk
import Network.RTorrent.Command.Internals
import Network.RTorrent.Priority
import Network.RTorrent.Value

--import Data.List.Split (splitOn)

data FileId = FileId !TorrentId !Int deriving Show

instance RpcType FileId where
    toValue (FileId (TorrentId tid) i) = ValueString $ tid <> ":f" <> T.pack (show i)
    fromValue v = uncurry FileId <$> (parse =<< fromValue v)
      where
        parse :: (Monad m, MonadFail m) => T.Text -> m (TorrentId, Int)
        parse str = do
            [hash, i] <- return $ T.splitOn ":f" str
            return (TorrentId hash, read (T.unpack i))

data FileInfo = FileInfo {
    filePath :: !T.Text
  , fileSizeBytes :: !Int
  , fileSizeChunks :: !Int
  , fileCompletedChunks :: !Int
  , filePriority :: !FilePriority
  , fileOffset :: !Int
  , fileId :: !FileId
} deriving Show

type FileAction = Action FileId

-- | Run the file action on all files that a torrent has selected by a list of regexps.
--
-- Note that the currently (as of 0.15.5) RTorrent regexps are very limited and only supports the wildcard @*@ matching any number of any characters.
allFilesFiltered :: [T.Text] -> (FileId -> FileAction a) -> TorrentId -> TorrentAction (V.Vector (FileId :*: a))
allFilesFiltered filters f = fmap addId . (getTorrentId <+> allToMulti (allF f))
  where
    addId (hash :*: files) =
        V.imap (\index -> (:*:) (FileId hash index)) files
    allF :: (FileId -> FileAction a) -> AllAction FileId a
    allF = AllAction (FileId (TorrentId "") 0) "f.multicall" (V.singleton fs)
    fs = if null filters then PString "" else PFilter filters
--
-- | Run the file action on all files that a torrent has.
allFiles :: (FileId -> FileAction a) -> TorrentId -> TorrentAction (V.Vector (FileId :*: a))
allFiles = allFilesFiltered []

-- | Get the file name relative to the torrent base directory.
getFilePath :: FileId -> FileAction T.Text
getFilePath = simpleAction "f.path" []

-- | Get the absolute path.
getFileAbsolutePath :: FileId -> FileAction T.Text
getFileAbsolutePath = simpleAction "f.frozen_path" []

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

-- | Get @FileInfo@ for @FileId@. The @FileId@ can be got by running @allFiles@.
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

-- | Get FileInfo for all files in a torrent filtered by a list of regexps.
getTorrentFileInfoFiltered :: [T.Text] -> TorrentId -> TorrentAction (V.Vector FileInfo)
getTorrentFileInfoFiltered filters = fmap (V.map contract) . allFilesFiltered filters getFilePartial
  where
    contract (x :*: f) = f x

-- | Get FileInfo for all files in a torrent.
getTorrentFileInfo :: TorrentId -> TorrentAction (V.Vector FileInfo)
getTorrentFileInfo = getTorrentFileInfoFiltered []
