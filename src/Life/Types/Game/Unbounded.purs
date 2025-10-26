module Life.Types.Game.Unbounded
  ( Unbounded
  )
  where

import Prelude

import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Data.Array as Array
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Life.Types.Cell (Cell)
import Life.Types.Cell as Cell
import Life.Types.Life (class Automaton, class CellularComonad, class InteractiveAutomaton, class Life, class CellularAutomaton, class VisibleAutomaton, comonadicGrid, comonadicSteps)
import Life.Utils (truthy)

newtype Unbounded a = Unbounded
  { cells :: Map Cell a
  , focus :: Cell
  , default :: a
  }
derive newtype instance Eq a => Eq (Unbounded a)

instance Functor Unbounded where
  map f (Unbounded u) = Unbounded u
    { cells = f <$> u.cells
    , default = f u.default
    }

instance Extend Unbounded where
  extend f (Unbounded ua) = Unbounded
    { cells: relevantCells # mapWithIndex \cell _ -> f (Unbounded ua { focus = cell })
    , focus: ua.focus
    , default: f (Unbounded ua { cells = Map.empty })
    }
    where
      livingCells = Map.filter truthy ua.cells

      relevantCells =
        livingCells
        # flip foldlWithIndex livingCells \cell acc _ ->
          Cell.neighbors cell
          # foldl (flip \neighbor -> Map.insertWith const neighbor ua.default) acc

instance Comonad Unbounded where
  extract (Unbounded u) =
    Map.lookup u.focus u.cells # fromMaybe u.default

instance CellularComonad Unbounded where
  focusCell cell (Unbounded u') = Unbounded u' { focus = cell }
  extractCell (Unbounded { focus }) = focus

instance Automaton Unbounded where
  steps = comonadicSteps

instance CellularAutomaton Unbounded where
  fromCells _ _ cells =
    cells
    # Array.fromFoldable
    <#> (_ /\ true)
    # Map.fromFoldable
    # Unbounded <<< { cells: _, focus: 0 /\ 0, default: false }

  toCells (Unbounded { cells }) = cells # Map.filter identity # Map.keys

instance VisibleAutomaton Unbounded where
  grid = comonadicGrid

instance InteractiveAutomaton Unbounded where
  update f row col (Unbounded u) = Unbounded u
    { cells = Map.alter (Just <<< f <<< fromMaybe u.default) (row /\ col) u.cells
    }

instance Life Unbounded where
  label = "unbounded"
  description = "even though only a finite portion is visible, cells off-screen can be alive and affect the cells in the visible grid"
