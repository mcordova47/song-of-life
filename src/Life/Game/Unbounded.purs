module Life.Game.Unbounded where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Foldable (foldl)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Life.Types.Cell (Cell)

step :: Set Cell -> Set Cell
step cells = cells # relevantCells # foldl stepCell cells
  where
    stepCell livingCells cell =
      case livingNeighbors cell of
        3 -> Set.insert cell livingCells
        2 -> livingCells
        _ -> Set.delete cell livingCells

    livingNeighbors cell =
      foldl countLiving 0 $ neighbors cell

    countLiving acc cell =
      if Set.member cell cells then acc + 1 else acc

    neighbors (row /\ col) = Set.fromFoldable do
      row' <- [row - 1, row, row + 1]
      col' <- [col - 1, col, col + 1]
      guard $ (row' /\ col') /= (row /\ col)
      pure (row' /\ col')

    relevantCells = cells # foldl \fullSet cell ->
      neighbors cell # Set.union fullSet

steps :: Int -> Set Cell -> Set Cell
steps n cells =
  cells
  # Array.fromFoldable
  <#> (\(row /\ col) -> { row, col })
  # runFn2 steps_ n
  <#> (\({ row, col }) -> row /\ col)
  # Set.fromFoldable

foreign import steps_ :: Fn2 Int (Array { row :: Int, col :: Int }) (Array { row :: Int, col :: Int })
