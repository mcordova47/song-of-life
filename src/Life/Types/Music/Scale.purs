module Life.Types.Music.Scale
  ( Scale
  , diatonic
  , hexatonic
  , pentatonic
  , scale
  )
  where

import Prelude

import Life.Types.Music.Degree (Degree)
import Life.Types.Music.Degree as Degree
import Life.Types.Music.Note (Note, (\\))
import Life.Types.Music.PitchClass (PitchClass)
import Life.Utils ((>>>>))

type Scale = PitchClass -> Int -> Array Note

scale :: Array Degree -> Scale
scale degrees key octave =
  degrees <@> key <@> (key \\ octave)

diatonic :: Scale
diatonic = scale
  [ Degree.tonal
  , Degree.third
  , Degree.fifth
  , Degree.second
  , Degree.seventh
  , Degree.fourth
  , Degree.sixth
  , Degree.octave
  , Degree.octave >>>> Degree.third
  , Degree.octave >>>> Degree.fifth
  , Degree.octave >>>> Degree.second
  , Degree.octave >>>> Degree.fourth
  , Degree.octave >>>> Degree.sixth
  , Degree.octave >>>> Degree.seventh
  ]

hexatonic :: Scale
hexatonic = scale
  [ Degree.tonal
  , Degree.third
  , Degree.fifth
  , Degree.second
  , Degree.fourth
  , Degree.sixth
  , Degree.octave
  , Degree.octave >>>> Degree.third
  , Degree.octave >>>> Degree.fifth
  , Degree.octave >>>> Degree.second
  , Degree.octave >>>> Degree.fourth
  , Degree.octave >>>> Degree.sixth
  , Degree.octave >>>> Degree.octave
  , Degree.octave >>>> Degree.octave >>>> Degree.third
  , Degree.octave >>>> Degree.octave >>>> Degree.fifth
  , Degree.octave >>>> Degree.octave >>>> Degree.second
  ]

pentatonic :: Scale
pentatonic = scale
  [ Degree.tonal
  , Degree.third
  , Degree.fifth
  , Degree.second
  , Degree.sixth
  , Degree.octave
  , Degree.octave >>>> Degree.third
  , Degree.octave >>>> Degree.fifth
  , Degree.octave >>>> Degree.second
  , Degree.octave >>>> Degree.sixth
  , Degree.octave >>>> Degree.octave
  , Degree.octave >>>> Degree.fifth
  , Degree.octave >>>> Degree.octave
  , Degree.octave >>>> Degree.octave >>>> Degree.third
  , Degree.octave >>>> Degree.octave >>>> Degree.fifth
  , Degree.octave >>>> Degree.octave >>>> Degree.second
  ]
