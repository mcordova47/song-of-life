module Life.Types.Game.Bounded
  ( Bounded
  , size
  )
  where

import Prelude

import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Data.Array ((!!), (..))
import Data.Array as Array
import Data.Foldable (foldl)
import Data.Maybe (fromMaybe)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Life.Types.Cell (Cell)
import Life.Types.Life (class Automaton, class CellularComonad, class InteractiveAutomaton, class Life, class CellularAutomaton, class VisibleAutomaton, comonadicGrid, comonadicSteps)
import Life.Utils as U

newtype Bounded a = Bounded
  { grid :: Array (Array a)
  , focus :: Cell
  , default :: a
  }

instance Functor Bounded where
  map f (Bounded b) =
    Bounded b { grid = map f <$> b.grid, default = f b.default }

instance Extend Bounded where
  extend f (Bounded b) = Bounded b { grid = grid', default = default }
    where
      grid' =
        b.grid # Array.mapWithIndex \row cols ->
          cols # Array.mapWithIndex \col _ ->
            f $ Bounded b { focus = row /\ col }

      default = f $ Bounded b { grid = [] }

instance Comonad Bounded where
  extract (Bounded b@{ focus: row /\ col }) =
    fromMaybe b.default (b.grid !! row >>= flip Array.index col)

instance CellularComonad Bounded where
  focusCell cell (Bounded b') = Bounded b' { focus = cell }
  extractCell (Bounded { focus }) = focus

instance Automaton Bounded where
  steps = comonadicSteps

instance CellularAutomaton Bounded where
  fromCells rows cols livingCells =
    Bounded { grid, focus: 0 /\ 0, default: false }
    where
      grid =
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
  grid = comonadicGrid

instance InteractiveAutomaton Bounded where
  update f row col (Bounded b) = Bounded b
    { grid = U.tryModifyAt row (U.tryModifyAt col f) b.grid }

instance Life Bounded where
  label = "bounded"
  description = "beyond the edges all cells are considered out of bounds and always dead, whereas it is often played on an infinite grid"

size :: forall a. Bounded a -> { rows :: Int, cols :: Int }
size (Bounded { grid }) = { rows, cols }
  where
    rows = Array.length grid
    cols = Array.head grid <#> Array.length # fromMaybe 0
