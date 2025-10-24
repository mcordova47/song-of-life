module Life.Types.Life
  ( class InteractiveLife
  , class Life
  , class VisibleLife
  , grid
  , neighbors
  , render
  , step
  , toggle
  , update
  )
  where

import Prelude

import Control.Comonad (class Comonad, extend, extract)
import Data.Foldable (foldMap)
import Life.Types.Rule as Rule

class Comonad f <= Life f where
  neighbors :: forall a. (a -> Boolean) -> f a -> Int

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

type RenderArgs f m =
  { life :: f Boolean
  , rows :: Int
  , cols :: Int
  , renderRow :: m -> m
  , renderCol :: Boolean -> m
  }

render :: forall f m. Monoid m => VisibleLife f => RenderArgs f m -> m
render args =
  args.life
  # grid args.rows args.cols
  # foldMap (foldMap args.renderCol >>> args.renderRow)
