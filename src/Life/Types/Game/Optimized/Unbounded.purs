module Life.Types.Game.Optimized.Unbounded where


import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Tuple.Nested (type (/\), (/\))
import Life.Game.Optimized.Unbounded as Optimized
import Life.Types.Cell (Cell)
import Life.Types.Life (class Automaton, class InteractiveAutomaton, class Life, class CellularAutomaton, class VisibleAutomaton)
import Life.Types.Rule as Rule
import Life.Utils.Array as A

newtype Unbounded a = Unbounded
  { cells :: Map Cell a
  , default :: a
  }
derive newtype instance Eq a => Eq (Unbounded a)

instance Automaton Unbounded where
  steps n rule (Unbounded u) =
    u.cells # Map.keys # Optimized.steps (Rule.fromNamed rule) n # fromCells

instance CellularAutomaton Unbounded where
  fromCells _ _ = fromCells

  toCells (Unbounded { cells }) = cells # Map.filter identity # Map.keys

instance VisibleAutomaton Unbounded where
  grid rows cols (Unbounded u) =
    A.grid rows cols <#> map \cell ->
      Map.lookup cell u.cells # fromMaybe u.default

instance InteractiveAutomaton Unbounded where
  update f row col (Unbounded u) = Unbounded u
    { cells = Map.alter (Just <<< f <<< fromMaybe u.default) (row /\ col) u.cells
    }

instance Life Unbounded where
  label = "unbounded"
  description = "even though only a finite portion is visible, cells off-screen can be alive and affect the cells in the visible grid"

fromCells :: Set (Int /\ Int) -> Unbounded Boolean
fromCells =
  Array.fromFoldable
  >>> map (_ /\ true)
  >>> Map.fromFoldable
  >>> { cells: _, default: false }
  >>> Unbounded
