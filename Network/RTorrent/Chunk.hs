{-|
Module      : Chunk
Copyright   : (c) Kai Lindholm, 2015
License     : MIT
Maintainer  : megantti@gmail.com
Stability   : experimental

-}

module Network.RTorrent.Chunk (
      convertChunks
    , convertChunksPad
) where

import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Text as T

-- | Convert a string representing bits to a list of booleans
-- and pads it to the correct length.
convertChunksPad :: Int -- ^ Total number of chunks
                 -> T.Text -- ^ A bitfield represented in hexadecimals
                 -> Maybe (V.Vector Bool)
convertChunksPad len = fmap (pad len) . convertChunks
  where
    pad :: Int -> V.Vector Bool -> V.Vector Bool
    pad i v = 
        if i > V.length v 
        then v <> V.replicate (i - V.length v) False
        else v

-- | Convert a string representing bits to a list of booleans.
convertChunks :: T.Text -- ^ A bitfield represented in hexadecimals
                -> Maybe (V.Vector Bool)
convertChunks T.Empty = Nothing
convertChunks str = fmap V.concat . mapM (fmap V.fromList . convert) $ T.unpack str
  where
    base2 = reverse . map (toEnum . (`mod` 2)) . take 4 . iterate (`div` 2)
    convert = fmap base2 . num
    num ch
      | '0' <= ch && ch <= '9' = Just $ fromEnum ch - fromEnum '0'
      | 'A' <= ch && ch <= 'F' = Just $ 10 + fromEnum ch - fromEnum 'A'
      | otherwise = Nothing
    
