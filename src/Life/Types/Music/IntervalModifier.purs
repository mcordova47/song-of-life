module Life.Types.Music.IntervalModifier
  ( IntervalModifier
  , augmented
  , diminished
  , major
  , minor
  , none
  , perfect
  , toHalfSteps
  , toModifier
  )
  where

import Prelude

import Life.Types.Music.Modifier (Modifier(..))

newtype IntervalModifier = IntervalModifier Int

none :: IntervalModifier
none = IntervalModifier 0

minor :: IntervalModifier
minor = IntervalModifier (-1)

major :: IntervalModifier
major = IntervalModifier 0

diminished :: IntervalModifier
diminished = IntervalModifier (-1)

perfect :: IntervalModifier
perfect = IntervalModifier 0

augmented :: IntervalModifier
augmented = IntervalModifier 1

toHalfSteps :: IntervalModifier -> Int
toHalfSteps (IntervalModifier n) = n

toModifier :: IntervalModifier -> Modifier
toModifier (IntervalModifier m) = Modifier m
