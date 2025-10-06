module Life.Music
  ( (<<)
  , (>>)
  , Note
  , Scale
  , Step
  , a4
  , addStep
  , halfStep
  , ionian
  , major2nd
  , major3rd
  , major6th
  , major7th
  , major9th
  , minor2nd
  , minor3rd
  , minor6th
  , minor7th
  , minor9th
  , nthNote
  , octave
  , pentatonic
  , perfect4th
  , perfect5th
  , subtractStep
  , tritone
  , wholeStep
  )
  where

import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.Foldable (fold)
import Data.Monoid (power)
import Data.Number as Number

newtype Note = Note Number

newtype Step = Step Number
instance Semigroup Step where
  append (Step a) (Step b) = Step $ a * b
instance Monoid Step where
  mempty = Step 1.0

type Scale = Array Step

a4 :: Note
a4 = Note 440.0

halfStep :: Step
halfStep = Step $ Number.pow 2.0 (1.0 / 12.0)

wholeStep :: Step
wholeStep = power halfStep 2

minor2nd :: Step
minor2nd = halfStep

major2nd :: Step
major2nd = wholeStep

minor3rd :: Step
minor3rd = power halfStep 3

major3rd :: Step
major3rd = power halfStep 4

perfect4th :: Step
perfect4th = power halfStep 5

tritone :: Step
tritone = power halfStep 6

perfect5th :: Step
perfect5th = power halfStep 7

minor6th :: Step
minor6th = power halfStep 8

major6th :: Step
major6th = power halfStep 9

minor7th :: Step
minor7th = power halfStep 10

major7th :: Step
major7th = power halfStep 11

octave :: Step
octave = Step 2.0

minor9th :: Step
minor9th = power halfStep 13

major9th :: Step
major9th = power halfStep 14

addStep :: Note -> Step -> Note
addStep (Note n) (Step s) = Note $ s * n

infixl 4 addStep as >>

subtractStep :: Note -> Step -> Note
subtractStep (Note n) (Step s) = Note $ n / s

infixl 4 subtractStep as <<

ionian :: Scale
ionian = [wholeStep, wholeStep, halfStep, wholeStep, wholeStep, wholeStep]

pentatonic :: Scale
pentatonic = [wholeStep, wholeStep, minor3rd, wholeStep]

nthNote :: Scale -> Note -> Int -> Note
nthNote scale note n
  | n == 0 = note
  | otherwise = note >> scaleStep >> additionalStep
    where
      scaleStep = fold (scale !! mod (n - 1) (Array.length scale))
      additionalStep =
        power octave $ n / Array.length scale
