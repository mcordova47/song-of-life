module Life.Debug
  ( showCells
  , showPreset
  )
  where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Tuple.Nested (type (/\), (/\))
import Life.Types.Music.Letter as Letter
import Life.Types.Music.Modifier (Modifier(..))
import Life.Types.Music.PitchClass ((//))
import Life.Types.Music.Wave as Wave
import Life.Types.Preset (Preset)
import Life.Types.Preset as Preset

showPreset :: Preset -> String
showPreset p = Array.fold
  [ "presetV1' { "
  , "key: " <> Letter.display letter <> " // " <> showModifier modifier
  , ", root: " <> show root
  , ", wave: Wave." <> Wave.display wave
  , " }\n"
  , showCells "  " cells
  ]
  where
    cells = Preset.livingCells p
    wave = Preset.wave p
    root = Preset.root p
    letter // modifier = Preset.key p
    showModifier (Modifier m)
      | m == 0 = "natural"
      | m == (-1) = "flat"
      | m == 1 = "sharp"
      | m < -1 = "Modifier (" <> show m <> ")"
      | otherwise = "Modifier " <> show m

showCells :: ∀ a b f. Show a => Show b => Foldable f => String -> f (a /\ b) -> String
showCells indent =
  Array.fromFoldable
  >>> Array.mapWithIndex (\i cell -> indent <> leftPad i <> displayCell cell <> "\n")
  >>> Array.fold
  >>> (_ <> indent <> "]")
  where
    displayCell (col /\ row) = show col <> " /\\ " <> show row
    leftPad i
      | i == 0 = "[ "
      | otherwise = ", "
