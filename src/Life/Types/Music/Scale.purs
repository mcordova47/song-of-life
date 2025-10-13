module Life.Types.Music.Scale
  ( Scale
  , ScaleInput
  , diatonic
  , fundamentalNotes
  , hexatonic
  , pentatonic
  , scale
  , shift
  )
  where

import Prelude

import Data.Array as Array
import Data.Foldable (fold)
import Life.Types.Music.Degree (Degree)
import Life.Types.Music.Degree as Degree
import Life.Types.Music.Note (Note, (\\))
import Life.Types.Music.PitchClass (PitchClass)
import Life.Utils ((>>>>))

type ScaleInput = { key :: PitchClass, root :: Int, length :: Int }

type Scale =
  { length :: Int
  , notes :: ScaleInput -> Array Note
  }

scale :: Array Degree -> Scale
scale degrees =
  { length: Array.length degrees
  , notes
  }
  where
    notes { key, root, length } =
      ( degrees
        # Array.replicate repeat
        # Array.mapWithIndex (\i -> map \deg -> Degree.pow i Degree.octave >>>> deg)
        # fold
        # Array.take length
      )
      <@> key
      <@> (key \\ root)
      where
        repeat
          | length <= 0 = 0
          | otherwise = length / Array.length degrees + 1

diatonic :: Scale
diatonic = scale
  [ Degree.tonal
  , Degree.third
  , Degree.fifth
  , Degree.second
  , Degree.seventh
  , Degree.fourth
  , Degree.sixth
  ]

hexatonic :: Scale
hexatonic = scale
  [ Degree.tonal
  , Degree.third
  , Degree.fifth
  , Degree.second
  , Degree.fourth
  , Degree.sixth
  ]

pentatonic :: Scale
pentatonic = scale
  [ Degree.tonal
  , Degree.third
  , Degree.fifth
  , Degree.second
  , Degree.sixth
  ]

fundamentalNotes :: Scale -> PitchClass -> Int -> Array Note
fundamentalNotes { notes, length } key root = notes { key, root, length }

shift :: Int -> Scale -> Scale
shift n scale' = scale' { notes = notes }
  where
    notes :: ScaleInput -> Array Note
    notes { key, root, length } =
      (before' <> after')
        # Array.replicate repeat
        # Array.mapWithIndex (\i -> map $ Degree.pow i Degree.octave key)
        # fold
        # Array.take length
      where
        scaleNotes = fundamentalNotes scale' key root

        { before, after } =
          Array.splitAt (mod n scale'.length) scaleNotes

        before' = after <#> shiftNote 0
        after' = before <#> shiftNote 1

        diff
          | n == 0 = 0
          | otherwise = n / Array.length scaleNotes

        shiftNote extra (p \\ o) =
          p \\ (o + diff + extra)

        repeat
          | length <= 0 = 0
          | otherwise = length / Array.length scaleNotes + 1
