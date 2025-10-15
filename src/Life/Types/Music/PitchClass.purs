module Life.Types.Music.PitchClass
  ( (//)
  , PitchClass(..)
  , all
  , codec
  , display
  , fix
  , halfSteps
  )
  where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Profunctor (dimap)
import Data.Tuple.Nested ((/\))
import Life.Types.Codec (class Serializable, Codec)
import Life.Types.Codec as Codec
import Life.Types.Music.Letter (Letter(..))
import Life.Types.Music.Letter as Letter
import Life.Types.Music.Modifier (Modifier, flat, natural, sharp)
import Life.Types.Music.Modifier as Modifier

data PitchClass
  = PitchClass Letter Modifier
derive instance Eq PitchClass
derive instance Generic PitchClass _

instance Show PitchClass where
  show = display

instance Serializable PitchClass where
  codec = codec

infixl 7 PitchClass as //

codec :: Codec String PitchClass
codec =
  dimap toTuple fromTuple $
    Codec.ljoin "." Letter.codec Modifier.codec
  where
    toTuple (l // m) = l /\ m
    fromTuple (l /\ m) = l // m

halfSteps :: PitchClass -> Int
halfSteps (letter // modifier) =
  Letter.halfSteps letter + Modifier.halfSteps modifier

fix :: PitchClass -> PitchClass -> PitchClass
fix (key // keyMod) (letter // _) =
  letter // (keyMod + Letter.modifier key letter)

display :: PitchClass -> String
display (l // m) =
  Letter.display l <> Modifier.display m

all :: Array PitchClass
all =
  [ C // natural
  , D // flat
  , D // natural
  , E // flat
  , E // natural
  , F // natural
  , F // sharp
  , G // natural
  , A // flat
  , A // natural
  , B // flat
  , B // natural
  ]
