module Life.Types.Life
  ( class InteractiveLife
  , class Life
  , class VisibleLife
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

class Comonad f <= Life f where
  label :: String
  description :: String
  neighbors :: forall a. (a -> Boolean) -> f a -> Int
  fromCells :: Int -> Int -> Set Cell -> f Boolean
  toCells :: f Boolean -> Set Cell

class Life f <= VisibleLife f where
  grid :: forall a. Int -> Int -> f a -> Array (Array a)

class VisibleLife f <= InteractiveLife f where
  update :: forall a. (a -> a) -> Int -> Int -> f a -> f a

step :: forall f. Life f => f Boolean -> f Boolean
step = extend rule
  where
    rule g = Rule.life (extract g) (neighbors identity g)

toggle :: forall f. InteractiveLife f => Int -> Int -> f Boolean -> f Boolean
toggle = update not

empty :: forall f. Life f => Int -> Int -> f Boolean
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

render :: forall f m. Monoid m => VisibleLife f => RenderArgs f m -> m
render args =
  args.life
  # grid args.rows args.cols
  # foldMap (foldMap args.renderCol >>> args.renderRow)

renderInteractive :: forall f m e. Monoid m => InteractiveLife f => RenderInteractiveArgs f m e -> m
renderInteractive args =
  args.life
  # grid args.rows args.cols
  # foldMapWithIndex \row ->
  foldMapWithIndex (\col living -> args.renderCol { living, onClick: \_ -> toggle row col args.life, col })
  >>> \content -> args.renderRow { row, content }
