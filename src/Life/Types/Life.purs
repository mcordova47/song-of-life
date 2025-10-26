module Life.Types.Life
  ( class Automaton
  , class CellularComonad
  , class InteractiveAutomaton
  , class Life
  , class CellularAutomaton
  , class VisibleAutomaton
  , comonadicGrid
  , comonadicStep
  , comonadicSteps
  , description
  , empty
  , extractCell
  , focusCell
  , fromCells
  , grid
  , label
  , neighbors
  , render
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
import Data.Foldable (foldMap, foldl)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Set (Set)
import Data.Set as Set
import Life.Types.Cell (Cell)
import Life.Types.Cell as Cell
import Life.Types.Rule (RuleType)
import Life.Types.Rule as Rule
import Life.Utils (times)
import Life.Utils as U

class Comonad f <= CellularComonad f where
  focusCell :: forall a. Cell -> f a -> f a
  extractCell :: forall a. f a -> Cell

class Automaton f where
  steps :: Int -> RuleType -> f Boolean -> f Boolean

class Automaton f <= CellularAutomaton f where
  fromCells :: Int -> Int -> Set Cell -> f Boolean
  toCells :: f Boolean -> Set Cell

class CellularAutomaton f <= VisibleAutomaton f where
  grid :: forall a. Int -> Int -> f a -> Array (Array a)

class VisibleAutomaton f <= InteractiveAutomaton f where
  update :: forall a. (a -> a) -> Int -> Int -> f a -> f a

class InteractiveAutomaton f <= Life f where
  label :: String
  description :: String

type RenderArgs f m = RenderArgs' f m m Boolean
type RenderInteractiveArgs f m e = RenderArgs' f m
  { row :: Int
  , content :: m
  }
  { living :: Boolean
  , col :: Int
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

empty :: forall f. CellularAutomaton f => Int -> Int -> f Boolean
empty rows cols = fromCells rows cols Set.empty

neighbors :: forall f. CellularComonad f => f Boolean -> Int
neighbors g =
  Cell.neighbors (extractCell g)
  # flip foldl 0 \acc cell ->
    if extract $ focusCell cell g
      then acc + 1
      else acc

render :: forall f m. Monoid m => VisibleAutomaton f => RenderArgs f m -> m
render args =
  args.life
  # grid args.rows args.cols
  # foldMap (foldMap args.renderCol >>> args.renderRow)

renderInteractive :: forall f m e. Monoid m => InteractiveAutomaton f => RenderInteractiveArgs f m e -> m
renderInteractive args =
  args.life
  # grid args.rows args.cols
  # foldMapWithIndex \row ->
  foldMapWithIndex (\col living -> args.renderCol { living, onClick: \_ -> toggle row col args.life, col })
  >>> \content -> args.renderRow { row, content }

comonadicGrid :: forall f a. CellularComonad f => CellularAutomaton f => Int -> Int -> f a -> Array (Array a)
comonadicGrid rows cols f =
  U.grid rows cols <#> map \cell ->
    extract $ focusCell cell f

comonadicStep :: forall f. CellularComonad f => RuleType -> f Boolean -> f Boolean
comonadicStep rule = extend \g -> (Rule.rule rule) (extract g) (neighbors g)

comonadicSteps :: forall f. CellularComonad f => Int -> RuleType -> f Boolean -> f Boolean
comonadicSteps n rule = n # times (comonadicStep rule)

step :: forall f. Automaton f => RuleType -> f Boolean -> f Boolean
step = steps 1
