module Life.Types.Life
  ( class Automaton
  , class CellularAutomaton
  , class InteractiveAutomaton
  , class Life
  , class VisibleAutomaton
  , description
  , empty
  , fromCells
  , grid
  , label
  , neighbors
  , render
  , renderInteractive
  , step
  , toCells
  , toggle
  , update
  )
  where

import Prelude

import Control.Comonad (class Comonad, extend, extract)
import Data.Foldable (foldMap)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Set (Set)
import Data.Set as Set
import Life.Types.Cell (Cell)
import Life.Types.Rule as Rule

class Comonad f <= Automaton f where
  neighbors :: forall a. (a -> Boolean) -> f a -> Int

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

step :: forall f. Automaton f => f Boolean -> f Boolean
step = extend rule
  where
    rule g = Rule.life (extract g) (neighbors identity g)

toggle :: forall f. InteractiveAutomaton f => Int -> Int -> f Boolean -> f Boolean
toggle = update not

empty :: forall f. CellularAutomaton f => Int -> Int -> f Boolean
empty rows cols = fromCells rows cols Set.empty

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
