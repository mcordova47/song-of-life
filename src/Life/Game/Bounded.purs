module Life.Game.Bounded
  ( Bounded
  )
  where

import Prelude

import Control.Alternative (guard)
import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Data.Array ((!!), (..))
import Data.Array as Array
import Data.Foldable (foldl)
import Data.Maybe (fromMaybe, maybe)
import Data.Set as Set
import Data.Tuple.Nested (type (/\), (/\))
import Life.Types.Life (class Automaton, class CellularAutomaton, class InteractiveAutomaton, class Life, class VisibleAutomaton)
import Life.Utils as U

newtype Bounded a = Bounded
  { grid :: Array (Array a)
  , focus :: Int /\ Int /\ a
  }
derive newtype instance Eq a => Eq (Bounded a)

instance Functor Bounded where
  map f (Bounded b@{ focus: row /\ col /\ x }) =
    Bounded { grid: map f <$> b.grid, focus: row /\ col /\ f x }

instance Extend Bounded where
  extend f (Bounded b@{ focus: r /\ c /\ _ }) = Bounded { grid: grid', focus }
    where
      grid' =
        b.grid # Array.mapWithIndex \row cols ->
          cols # Array.mapWithIndex \col x ->
            f $ Bounded { grid: b.grid, focus: row /\ col /\ x }

      focus = r /\ c /\ f (Bounded b)

instance Comonad Bounded where
  extract (Bounded { focus: _ /\ _ /\ x }) = x

instance Automaton Bounded where
  neighbors p (Bounded b@{ focus: row /\ col /\ _ }) =
    Array.length $ Array.filter identity do
      row' <- [row - 1, row, row + 1]
      col' <- [col - 1, col, col + 1]
      guard (row /= row' || col /= col')
      pure $ (b.grid !! row' # fromMaybe []) !! col' # maybe false p

instance CellularAutomaton Bounded where
  fromCells rows cols livingCells =
    Bounded { grid: grid', focus: 0 /\ 0 /\ focused 0 0 }
    where
      grid' =
        (0 .. (rows - 1)) <#> \row ->
          (0 .. (cols - 1)) <#> \col ->
            focused row col

      focused row col =
        Set.member (row /\ col) livingCells

  toCells (Bounded b) = foldGrid indexedGrid
    where
      foldGrid = Set.empty # foldl \acc ->
        acc # foldl \acc' (row /\ col /\ living) ->
          if living then Set.insert (row /\ col) acc' else acc'

      indexedGrid =
        b.grid # Array.mapWithIndex \row cols ->
          cols # Array.mapWithIndex \col living ->
            (row /\ col /\ living)

instance VisibleAutomaton Bounded where
  grid rows cols (Bounded b) =
    Array.take cols <$> Array.take rows b.grid

instance InteractiveAutomaton Bounded where
  update f row col (Bounded b) = Bounded b
    { grid = U.tryModifyAt row (U.tryModifyAt col f) b.grid }

instance Life Bounded where
  label = "bounded"
  description = "beyond the edges all cells are considered out of bounds and always dead, whereas it is often played on an infinite grid"