module Life.Types.Life
  ( class CellularAutomaton
  , class InteractiveAutomaton
  , class Life
  , class TangibleAutomaton
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
  , step'
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
import Life.Types.Rule (Rule)
import Life.Types.Rule as Rule
import Life.Utils as U

class Comonad f <= CellularAutomaton f where
  focusCell :: forall a. Cell -> f a -> f a
  extractCell :: forall a. f a -> Cell

class CellularAutomaton f <= TangibleAutomaton f where
  fromCells :: Int -> Int -> Set Cell -> f Boolean
  toCells :: f Boolean -> Set Cell

class TangibleAutomaton f <= InteractiveAutomaton f where
  update :: forall a. (a -> a) -> Int -> Int -> f a -> f a

class InteractiveAutomaton f <= Life f where
  label :: String
  description :: String

step :: forall f. CellularAutomaton f => f Boolean -> f Boolean
step = step' Rule.default

step' :: forall f. CellularAutomaton f => Rule -> f Boolean -> f Boolean
step' rule = extend \g -> rule (extract g) (neighbors g)

toggle :: forall f. InteractiveAutomaton f => Int -> Int -> f Boolean -> f Boolean
toggle = update not

empty :: forall f. TangibleAutomaton f => Int -> Int -> f Boolean
empty rows cols = fromCells rows cols Set.empty

neighbors :: forall f. CellularAutomaton f => f Boolean -> Int
neighbors g =
  Cell.neighbors (extractCell g)
  # flip foldl 0 \acc cell ->
    if extract $ focusCell cell g
      then acc + 1
      else acc

grid :: forall f a. CellularAutomaton f => Int -> Int -> f a -> Array (Array a)
grid rows cols f =
  U.grid rows cols <#> map \cell ->
    extract $ focusCell cell f

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

render :: forall f m. Monoid m => CellularAutomaton f => RenderArgs f m -> m
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
