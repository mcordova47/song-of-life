module Life.Game.Unbounded where

import Prelude

import Data.Array as Array
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Life.Types.Cell (Cell)

steps :: Int -> Set Cell -> Set Cell
steps n cells =
  cells
  # Array.fromFoldable
  <#> (\(row /\ col) -> { row, col })
  # runFn2 steps_ n
  <#> (\({ row, col }) -> row /\ col)
  # Set.fromFoldable

foreign import steps_ :: Fn2 Int (Array { row :: Int, col :: Int }) (Array { row :: Int, col :: Int })
