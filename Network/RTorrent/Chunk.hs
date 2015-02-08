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

-- | Convert a string representing bits to a list of booleans
-- and pads it to the correct length.
convertChunksPad :: Int -- ^ Total number of chunks
                 -> String -- ^ A bitfield represented in hexadecimals
                 -> Maybe [Bool]
convertChunksPad len = fmap (pad len) . convertChunks
  where
    pad i str 
      | i <= 0 = []
      | otherwise = case str of
          []     -> replicate i False
          (x:xs) -> x : pad (i - 1) xs

-- | Convert a string representing bits to a list of booleans.
convertChunks :: String -- ^ A bitfield represented in hexadecimals
                -> Maybe [Bool]
convertChunks [] = Nothing
convertChunks str = fmap concat . mapM convert $ str
  where
    base2 = reverse . map (toEnum . (`mod` 2)) . take 4 . iterate (`div` 2)
    convert = fmap base2 . num
    num ch
      | '0' <= ch && ch <= '9' = Just $ fromEnum ch - fromEnum '0'
      | 'A' <= ch && ch <= 'F' = Just $ 10 + fromEnum ch - fromEnum 'A'
      | otherwise = Nothing
    
