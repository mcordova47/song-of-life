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
import Data.Codec as C
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Life.Types.Codec (Codec)
import Life.Types.Music.Modifier (Modifier, flat, natural, sharp)

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
codec = C.codec decode encode
  where
    decode = case _ of
      "C" -> Just C
      "D" -> Just D
      "E" -> Just E
      "F" -> Just F
      "G" -> Just G
      "A" -> Just A
      "B" -> Just B
      _ -> Nothing

    encode = case _ of
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
