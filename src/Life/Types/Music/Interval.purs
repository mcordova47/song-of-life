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

-- | An interval is a degree in the the major scale combined with a modifier,
-- | like minor or augmented. So a major third is just the third degree without
-- | a modifier and a diminished fifth is the fifth with the diminished modifier
-- | which subtracts one half-step.
data Interval = Interval Degree IntervalModifier

addTo :: Note -> Interval -> Note
addTo note@(pitchClass \\ _) (Interval degree intMod) = l // (m + I.toModifier intMod) \\ o
  where
    (l // m \\ o) = degree pitchClass note
