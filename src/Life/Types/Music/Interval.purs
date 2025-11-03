module Life.Types.Music.Interval
  ( Interval(..)
  , addTo
  )
  where

import Prelude

import Life.Types.Music.Degree (Degree)
import Life.Types.Music.IntervalModifier (IntervalModifier)
import Life.Types.Music.IntervalModifier as I
import Life.Types.Music.Note (Note, (\\))
import Life.Types.Music.PitchClass ((//))

data Interval = Interval Degree IntervalModifier

addTo :: Note -> Interval -> Note
addTo note@(pitchClass \\ _) (Interval degree intMod) = l // (m + I.toModifier intMod) \\ o
  where
    (l // m \\ o) = degree pitchClass note
