module Life.Types.Music.Scale
  ( Scale
  , diatonic
  , hexatonic
  , pentatonic
  , scale
  )
  where

import Prelude

import Life.Types.Music.Note (Degree, Note, (\\))
import Life.Types.Music.Note as Note
import Life.Types.Music.PitchClass (PitchClass)
import Life.Utils ((>>>>))

type Scale = PitchClass -> Int -> Array Note

scale :: Array Degree -> Scale
scale degrees key octave =
  degrees <@> key <@> (key \\ octave)

diatonic :: Scale
diatonic = scale
  [ Note.tonal
  , Note.third
  , Note.fifth
  , Note.second
  , Note.seventh
  , Note.fourth
  , Note.sixth
  , Note.octave
  , Note.octave >>>> Note.third
  , Note.octave >>>> Note.fifth
  , Note.octave >>>> Note.second
  , Note.octave >>>> Note.fourth
  , Note.octave >>>> Note.sixth
  , Note.octave >>>> Note.seventh
  ]

hexatonic :: Scale
hexatonic = scale
  [ Note.tonal
  , Note.third
  , Note.fifth
  , Note.second
  , Note.fourth
  , Note.sixth
  , Note.octave
  , Note.octave >>>> Note.third
  , Note.octave >>>> Note.fifth
  , Note.octave >>>> Note.second
  , Note.octave >>>> Note.fourth
  , Note.octave >>>> Note.sixth
  , Note.octave >>>> Note.octave
  , Note.octave >>>> Note.octave >>>> Note.third
  , Note.octave >>>> Note.octave >>>> Note.fifth
  , Note.octave >>>> Note.octave >>>> Note.second
  ]

pentatonic :: Scale
pentatonic = scale
  [ Note.tonal
  , Note.third
  , Note.fifth
  , Note.second
  , Note.sixth
  , Note.octave
  , Note.octave >>>> Note.third
  , Note.octave >>>> Note.fifth
  , Note.octave >>>> Note.second
  , Note.octave >>>> Note.sixth
  , Note.octave >>>> Note.octave
  , Note.octave >>>> Note.fifth
  , Note.octave >>>> Note.octave
  , Note.octave >>>> Note.octave >>>> Note.third
  , Note.octave >>>> Note.octave >>>> Note.fifth
  , Note.octave >>>> Note.octave >>>> Note.second
  ]
