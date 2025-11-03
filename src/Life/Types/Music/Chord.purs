module Life.Types.Music.Chord
  ( Chord(..)
  , defaultVoicing
  , display
  , intervals
  , notes
  , play
  )
  where

import Prelude

import Data.FoldableWithIndex (forWithIndex_)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Life.Types.Codec (class Serializable)
import Life.Types.Codec as Codec
import Life.Types.Music.Interval as I
import Life.Types.Music.NamedInterval (NamedInterval)
import Life.Types.Music.NamedInterval as NI
import Life.Types.Music.Note (Note, (\\))
import Life.Types.Music.Note as N
import Life.Types.Music.PitchClass (PitchClass)
import Life.Types.Music.Wave (Wave)

data Chord
  = Diminished
  | Minor
  | Major
  | Augmented
  | Sus2
  | Sus4
  | Minor6
  | Major6
  | Minor7
  | Dominant7
  | Major7
derive instance Generic Chord _

instance Serializable Chord where
  codec = Codec.enum case _ of
    Diminished -> "dim"
    Minor -> "min"
    Major -> "maj"
    Augmented -> "aug"
    Sus2 -> "sus2"
    Sus4 -> "sus4"
    Minor6 -> "min6"
    Major6 -> "maj6"
    Minor7 -> "min7"
    Dominant7 -> "dom7"
    Major7 -> "maj7"

intervals :: Chord -> Array NamedInterval
intervals = case _ of
  Diminished -> [NI.Unison, NI.Third NI.Minor, NI.Fifth NI.Diminished]
  Minor -> [NI.Unison, NI.Third NI.Minor, NI.Fifth NI.Perfect]
  Major -> [NI.Unison, NI.Third NI.Major, NI.Fifth NI.Perfect]
  Augmented -> [NI.Unison, NI.Third NI.Major, NI.Fifth NI.Augmented]
  Sus2 -> [NI.Unison, NI.Second NI.Major, NI.Fifth NI.Perfect]
  Sus4 -> [NI.Unison, NI.Fourth NI.Perfect, NI.Fifth NI.Perfect]
  Minor6 -> [NI.Unison, NI.Third NI.Minor, NI.Fifth NI.Perfect, NI.Sixth NI.Minor]
  Major6 -> [NI.Unison, NI.Third NI.Major, NI.Fifth NI.Perfect, NI.Sixth NI.Major]
  Minor7 -> [NI.Unison, NI.Third NI.Minor, NI.Fifth NI.Perfect, NI.Seventh NI.Minor]
  Dominant7 -> [NI.Unison, NI.Third NI.Major, NI.Fifth NI.Perfect, NI.Seventh NI.Minor]
  Major7 -> [NI.Unison, NI.Third NI.Major, NI.Fifth NI.Perfect, NI.Seventh NI.Major]

notes :: PitchClass -> Int -> Chord -> Array Note
notes pitchClass octave =
  intervals >>>
  map NI.toInterval >>>
  map (I.addTo (pitchClass \\ octave))

type Voicing = Int -> Note -> Note

defaultVoicing :: Voicing
defaultVoicing i (pc \\ o)
  | Int.even i = pc \\ o
  | otherwise = pc \\ (o + 1)

play :: Milliseconds -> Wave -> PitchClass -> Int -> Chord -> Voicing -> Effect Unit
play dur wave pitchClass octave chord voicing =
  forWithIndex_ (notes pitchClass octave chord) \i note ->
    N.play dur wave $ voicing i note

display ∷ Chord → String
display = case _ of
  Diminished -> "°"
  Minor -> "m"
  Major -> ""
  Augmented -> "+"
  Sus2 -> "sus2"
  Sus4 -> "sus4"
  Minor6 -> "m6"
  Major6 -> "6"
  Minor7 -> "m7"
  Dominant7 -> "7"
  Major7 -> "maj7"
