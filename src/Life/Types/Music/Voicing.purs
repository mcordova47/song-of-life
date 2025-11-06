module Life.Types.Music.Voicing
  ( Voicing
  , default
  , spaced
  )
  where

import Prelude

import Data.Int as Int
import Life.Types.Music.Note (Note, (\\))

type Voicing = Int -> Note -> Note

default :: Voicing
default = const identity

spaced :: Voicing
spaced i (pc \\ o)
  | Int.even i = pc \\ o
  | otherwise = pc \\ (o + 1)
