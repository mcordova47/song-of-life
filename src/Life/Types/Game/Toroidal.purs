module Life.Types.Game.Toroidal
  ( Toroidal
  )
  where

import Prelude

import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Data.Tuple.Nested ((/\))
import Life.Types.Cell (Cell)
import Life.Types.Game.Bounded (Bounded)
import Life.Types.Game.Bounded as Bounded
import Life.Types.Life (class CellularAutomaton, class CellularComonad, class InteractiveAutomaton, class Life, class TangibleAutomaton, class VisibleAutomaton, comonadicGrid, comonadicSteps, extractCell, focusCell, fromCells, toCells, update)

newtype Toroidal a = Toroidal (Bounded a)
derive newtype instance Functor Toroidal
derive newtype instance Extend Toroidal
derive newtype instance Comonad Toroidal

instance CellularComonad Toroidal where
  focusCell cell t@(Toroidal b) = Toroidal $ focusCell (cell # modulo t) b
  extractCell (Toroidal b) = extractCell b

instance CellularAutomaton Toroidal where
  steps = comonadicSteps

instance TangibleAutomaton Toroidal where
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
