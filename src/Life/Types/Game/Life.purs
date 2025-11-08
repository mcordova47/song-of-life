-- | This module contains all of the typeclasses which define different
-- | algorithms for cellular automata.
module Life.Types.Game.Life
  ( class Automaton
  , class CellularAutomaton
  , class CellularComonad
  , class InteractiveAutomaton
  , class Life
  , class VisibleAutomaton
  , comonadicGrid
  , comonadicStep
  , comonadicSteps
  , description
  , empty
  , extractCell
  , focusCell
  , foldMap
  , foldMapWithIndex
  , fromCells
  , grid
  , label
  , neighbors
  , renderInteractive
  , step
  , steps
  , toCells
  , toggle
  , update
  )
  where

import Prelude

import Control.Comonad (class Comonad, extend, extract)
import Data.Foldable as F
import Data.FoldableWithIndex as FI
import Data.Set (Set)
import Data.Set as Set
import Life.Types.Game.NamedRule (NamedRule)
import Life.Types.Game.Rule as Rule
import Life.Types.Grid.Cell (Cell)
import Life.Types.Grid.Cell as Cell
import Life.Utils.Array as A
import Life.Utils.Function (times)

-- | If an implementation can be expressed as a comonad with a focused cell, it
-- | gets free instances for Automaton and VisibleAutomaton.
class Comonad f <= CellularComonad f where
  focusCell :: forall a. Cell -> f a -> f a
  extractCell :: forall a. f a -> Cell

-- | This is the core typeclass that defines an algorithm for cellular automata.
class Automaton f where
  steps :: Int -> NamedRule -> f Boolean -> f Boolean

-- | An automata which can be transformed to/from a set of cells.
class Automaton f <= CellularAutomaton f where
  fromCells :: Int -> Int -> Set Cell -> f Boolean
  toCells :: f Boolean -> Set Cell

-- | This provides a way to render an automaton as a grid of cell states.
class CellularAutomaton f <= VisibleAutomaton f where
  grid :: forall a. Int -> Int -> f a -> Array (Array a)

-- | This provides a function for updating a given cell.
class VisibleAutomaton f <= InteractiveAutomaton f where
  update :: forall a. (a -> a) -> Int -> Int -> f a -> f a

-- | Adds metadata to describe the different implementations.
class InteractiveAutomaton f <= Life f where
  label :: String
  description :: String

type RenderArgs f m = RenderArgs' f m m Boolean
type RenderWithIndexArgs f m = RenderArgs' f m
  { row :: Int
  , content :: m
  }
  { col :: Int
  , row :: Int
  , living :: Boolean
  }
type RenderInteractiveArgs f m e = RenderArgs' f m
  { row :: Int
  , content :: m
  }
  { col :: Int
  , row :: Int
  , living :: Boolean
  , onClick :: e -> f Boolean
  }
type RenderArgs' f m r c =
  { life :: f Boolean
  , rows :: Int
  , cols :: Int
  , renderRow :: r -> m
  , renderCol :: c -> m
  }

toggle :: forall f. InteractiveAutomaton f => Int -> Int -> f Boolean -> f Boolean
toggle = update not

empty :: forall @f. CellularAutomaton f => Int -> Int -> f Boolean
empty rows cols = fromCells rows cols Set.empty

neighbors :: forall f. CellularComonad f => f Boolean -> Int
neighbors g =
  Cell.neighbors (extractCell g)
  # flip F.foldl 0 \acc cell ->
    if extract $ focusCell cell g
      then acc + 1
      else acc

foldMap :: forall f m. Monoid m => VisibleAutomaton f => RenderArgs f m -> m
foldMap args =
  foldMapWithIndex args
    { renderRow =  args.renderRow <<< _.content
    , renderCol = args.renderCol <<< _.living
    }

foldMapWithIndex :: forall f m. Monoid m => VisibleAutomaton f => RenderWithIndexArgs f m -> m
foldMapWithIndex args =
  args.life
  # grid args.rows args.cols
  # FI.foldMapWithIndex \row ->
  FI.foldMapWithIndex (\col living -> args.renderCol { living, row, col })
  >>> \content -> args.renderRow { row, content }

renderInteractive :: forall f m e. Monoid m => InteractiveAutomaton f => RenderInteractiveArgs f m e -> m
renderInteractive args =
  foldMapWithIndex args
    { renderCol = \{ row, col, living } ->
        args.renderCol { row, col, living, onClick: \_ -> toggle row col args.life }
    }

comonadicGrid :: forall f a. CellularComonad f => CellularAutomaton f => Int -> Int -> f a -> Array (Array a)
comonadicGrid rows cols f =
  A.grid rows cols <#> map \cell ->
    extract $ focusCell cell f

comonadicStep :: forall f. CellularComonad f => NamedRule -> f Boolean -> f Boolean
comonadicStep rule = extend \g -> (Rule.fromNamed rule) (extract g) (neighbors g)

comonadicSteps :: forall f. CellularComonad f => Int -> NamedRule -> f Boolean -> f Boolean
comonadicSteps n rule = n # times (comonadicStep rule)

step :: forall f. Automaton f => NamedRule -> f Boolean -> f Boolean
step = steps 1
