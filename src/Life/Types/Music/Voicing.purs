module Life.Types.Music.Voicing
  ( Voicing
  , default
  , spaced
  )
  where

import Prelude

import Data.Int as Int
import Life.Types.Music.Note (Note, (\\))

-- | For now, a voicing just says "given this note position, this is how the
-- | note should be phrased."
type Voicing = Int -> Note -> Note

default :: Voicing
default = const identity

spaced :: Voicing
spaced i (pc \\ o)
  | Int.even i = pc \\ o
  | otherwise = pc \\ (o + 1)
