module Life.Game.Optimized.Unbounded where

import Prelude

import Data.Array as Array
import Data.Function.Uncurried (Fn2, Fn3, mkFn2, runFn3)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Life.Types.Grid.Cell (Cell)

steps :: (Boolean -> Int -> Boolean) -> Int -> Set Cell -> Set Cell
steps rule n cells =
  cells
  # Array.fromFoldable
  <#> (\(row /\ col) -> { row, col })
  # runFn3 steps_ (mkFn2 rule) n
  <#> (\({ row, col }) -> row /\ col)
  # Set.fromFoldable

foreign import steps_ :: Fn3 (Fn2 Boolean Int Boolean) Int (Array { row :: Int, col :: Int }) (Array { row :: Int, col :: Int })
