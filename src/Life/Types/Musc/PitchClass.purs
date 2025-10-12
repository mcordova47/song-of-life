module Life.Types.Musc.PitchClass
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

import Data.Profunctor (dimap)
import Data.Tuple.Nested ((/\))
import Life.Types.Codec (Codec)
import Life.Types.Codec as Codec
import Life.Types.Musc.Letter (Letter(..))
import Life.Types.Musc.Letter as Letter
import Life.Types.Musc.Modifier (Modifier, natural, sharp)
import Life.Types.Musc.Modifier as Modifier

data PitchClass
  = PitchClass Letter Modifier
derive instance Eq PitchClass

infixl 7 PitchClass as //

codec :: Codec String PitchClass
codec =
  dimap toTuple fromTuple $
    Codec.ljoin "_" Letter.codec Modifier.codec
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
  , C // sharp
  , D // natural
  , D // sharp
  , E // natural
  , F // natural
  , F // sharp
  , G // natural
  , G // sharp
  , A // natural
  , A // sharp
  , B // natural
  ]
