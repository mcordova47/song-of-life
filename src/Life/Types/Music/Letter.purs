module Life.Types.Music.Letter
  ( Letter(..)
  , codec
  , degree
  , display
  , halfSteps
  , modifier
  )
  where

import Prelude hiding (degree)

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Life.Types.Codec (Codec)
import Life.Types.Codec as Codec
import Life.Types.Music.Modifier (Modifier, flat, natural, sharp)

-- | All of the letter notes (without sharp/flat modifiers) in Western music.
data Letter
  = C
  | D
  | E
  | F
  | G
  | A
  | B
derive instance Eq Letter
derive instance Generic Letter _

codec :: Codec String Letter
codec = Codec.enum case _ of
  C -> "C"
  D -> "D"
  E -> "E"
  F -> "F"
  G -> "G"
  A -> "A"
  B -> "B"

halfSteps :: Letter -> Int
halfSteps = case _ of
  C -> 0
  D -> 2
  E -> 4
  F -> 5
  G -> 7
  A -> 9
  B -> 11

degree :: Letter -> Int
degree = case _ of
  C -> 1
  D -> 2
  E -> 3
  F -> 4
  G -> 5
  A -> 6
  B -> 7

-- | For a given key, this returns the modifier for a given letter in that key.
-- | E.g. B is flat in the key of F.
modifier :: Letter -> Letter -> Modifier
modifier key letter =
  if isSharp then
    sharp
  else if isFlat then
    flat
  else
    natural
  where
    isSharp = case letter of
      F -> Array.notElem key [F, C]
      C -> Array.notElem key [F, C, G]
      G -> Array.notElem key [F, C, G, D]
      D -> Array.notElem key [F, C, G, D, A]
      A -> Array.notElem key [F, C, G, D, A, E]
      E -> Array.notElem key [F, C, G, D, A, E, B]
      B -> false

    isFlat = case letter of
      B -> key == F
      _ -> false

display :: Letter -> String
display = case _ of
  C -> "C"
  D -> "D"
  E -> "E"
  F -> "F"
  G -> "G"
  A -> "A"
  B -> "B"
