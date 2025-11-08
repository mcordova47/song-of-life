module Life.Types.Game.Engines.Toroidal
  ( Toroidal
  )
  where

import Prelude

import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Data.Tuple.Nested ((/\))
import Life.Types.Game.Engines.Bounded (Bounded)
import Life.Types.Game.Engines.Bounded as Bounded
import Life.Types.Game.Life (class Automaton, class CellularComonad, class InteractiveAutomaton, class Life, class CellularAutomaton, class VisibleAutomaton, comonadicGrid, comonadicSteps, extractCell, focusCell, fromCells, toCells, update)
import Life.Types.Grid.Cell (Cell)

-- | A comonadic implementation of an automaton on a torus. I.e. the left and
-- | right sides of the grid are connected, as are the top and bottom. A glider
-- | would endlessly loop, exiting one boundary and entering at another. The
-- | representation is based on the Bounded implementation, the only difference
-- | being that when a cell is focused, it's focused modulo the bounds.
newtype Toroidal a = Toroidal (Bounded a)
derive newtype instance Eq a => Eq (Toroidal a)
derive newtype instance Functor Toroidal
derive newtype instance Extend Toroidal
derive newtype instance Comonad Toroidal

instance CellularComonad Toroidal where
  focusCell cell t@(Toroidal b) = Toroidal $ focusCell (cell # modulo t) b
  extractCell (Toroidal b) = extractCell b

instance Automaton Toroidal where
  steps = comonadicSteps

instance CellularAutomaton Toroidal where
  fromCells rows cols cells =
    Toroidal $ fromCells rows cols cells

  toCells (Toroidal b) = toCells b

instance VisibleAutomaton Toroidal where
  grid = comonadicGrid

instance InteractiveAutomaton Toroidal where
  update f row col (Toroidal b) =
    Toroidal $ update f row col b

instance Life Toroidal where
  label = "toroidal"
  description = "the grid “wraps around” as if rolled into a torus (bottom cells are neighbors of top cells and left cells are neighbors of right cells)"

modulo :: forall a. Toroidal a -> Cell -> Cell
modulo (Toroidal b) (row /\ col) = mod row rows /\ mod col cols
  where
    { rows, cols } = Bounded.size b
