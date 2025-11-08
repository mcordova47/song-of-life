module Life.Utils.Math where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Number (sqrt)
import Data.Semigroup.Foldable (minimumBy)
import Data.Tuple.Nested ((/\))
import Life.Types.Grid.Cell (Cell)

-- | A discrete geometric median of cells
geometricMedian :: NonEmptyArray Cell -> Cell
geometricMedian cells =
  minimumBy (comparing totalDistance) cells
  where
    totalDistance c =
      foldl (\acc c' -> acc + distance c c') 0.0 cells

    distance (r1 /\ c1) (r2 /\ c2) =
      let dr = Int.toNumber (r1 - r2)
          dc = Int.toNumber (c1 - c2)
      in sqrt (dr * dr + dc * dc)
