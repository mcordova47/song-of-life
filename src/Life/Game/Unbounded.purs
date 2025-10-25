module Life.Game.Unbounded
  ( Unbounded(..)
  )
  where

import Prelude

import Control.Alternative (guard)
import Control.Comonad (class Comonad, extract)
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
import Life.Types.Life (class InteractiveLife, class Life, class VisibleLife)
import Life.Utils (truthy)
import Life.Utils as U

newtype Unbounded a = Unbounded
  { cells :: Map Cell a
  , focused :: Cell
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
    { cells: relevantCells # mapWithIndex \cell _ -> f (Unbounded ua { focused = cell })
    , focused: ua.focused
    , default: f (Unbounded ua { focused = outOfBounds })
    }
    where
      livingCells = Map.filter truthy ua.cells

      relevantCells =
        livingCells
        # flip foldlWithIndex livingCells \cell acc _ ->
          neighboringCells cell
          # foldl (flip \neighbor -> Map.insertWith const neighbor ua.default) acc

      outOfBounds = U.infinity /\ U.infinity

instance Comonad Unbounded where
  extract (Unbounded u) =
    Map.lookup u.focused u.cells # fromMaybe u.default

instance Life Unbounded where
  label = "unbounded"
  description = "even though only a finite portion is visible, cells off-screen can be alive and affect the cells in the visible grid"

  neighbors p (Unbounded u) = 
    neighboringCells u.focused
    # flip foldl 0 \acc cell ->
      if p $ extract $ Unbounded u { focused = cell }
        then acc + 1
        else acc

  fromCells _ _ cells =
    cells
    # Array.fromFoldable
    <#> (_ /\ true)
    # Map.fromFoldable
    # Unbounded <<< { cells: _, focused: 0 /\ 0, default: false }

  toCells (Unbounded { cells }) = cells # Map.filter identity # Map.keys

instance VisibleLife Unbounded where
  grid rows cols (Unbounded u) =
    U.grid rows cols <#> map \cell ->
      extract (Unbounded u { focused = cell })

instance InteractiveLife Unbounded where
  update f row col (Unbounded u) = Unbounded u
    { cells = Map.alter (Just <<< f <<< fromMaybe u.default) (row /\ col) u.cells
    }

neighboringCells :: Cell -> Array Cell
neighboringCells (row /\ col) = do
  row' <- [row - 1, row, row + 1]
  col' <- [col - 1, col, col + 1]
  guard (row' /= row || col' /= col)
  pure (row' /\ col')
