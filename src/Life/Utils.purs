module Life.Utils
  ( (>>>>)
  , chunksOf
  , compose2
  , fill
  , grid
  , infinity
  , randomTag
  , scrollIntoView
  , tags
  , transpose
  , truthy
  , tryModifyAt
  )
  where

import Prelude

import Data.Array ((..))
import Data.Array as Array
import Data.Bounded.Generic (class GenericBottom, class GenericTop, genericBottom, genericTop)
import Data.Enum.Generic (class GenericBoundedEnum, class GenericEnum, genericFromEnum, genericSucc, genericToEnum)
import Data.Function.Uncurried (Fn1, runFn1)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (class Unfoldable1, unfoldr1)
import Effect (Effect)
import Effect.Random as R
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Life.Types.Cell (Cell)

tags :: forall f a rep
  . Generic a rep
  => GenericBottom rep
  => GenericEnum rep
  => GenericBoundedEnum rep
  => Unfoldable1 f
  => f a
tags = genericBottom # unfoldr1 \w -> (w /\ genericSucc w)

randomTag :: forall @a rep
  . Generic a rep
  => GenericBottom rep
  => GenericTop rep
  => GenericBoundedEnum rep
  => Effect a
randomTag =
  R.randomInt
    (genericFromEnum (genericBottom :: a))
    (genericFromEnum (genericTop :: a))
    <#> genericToEnum
    <#> fromMaybe genericBottom

compose2 :: forall a b c d. (d -> a -> b) -> (d -> b -> c) -> d -> a -> c
compose2 f g d = f d >>> g d

infixr 9 compose2 as >>>>

chunksOf :: forall a. Int -> Array a -> Array (Array a)
chunksOf n xs = Array.splitAt n xs # go
  where
    go { before, after }
      | Array.null before = []
      | Array.null after = [before]
      | otherwise = [before] <> chunksOf n after

fill :: forall a. Int -> a -> Array a -> Array a
fill n x xs =
  xs <> Array.replicate (n - Array.length xs) x

tryModifyAt :: forall a. Int -> (a -> a) -> Array a -> Array a
tryModifyAt i g xs = Array.modifyAt i g xs # fromMaybe xs

grid :: Int -> Int -> Array (Array Cell)
grid numRows numCols =
  rows <#> \row -> cols <#> \col -> row /\ col
  where
    rows = 0 .. (numRows - 1)
    cols = 0 .. (numCols - 1)

transpose :: forall a. Array (Array a) -> Array (Array a)
transpose rows = case Array.head rows of
  Just row -> transpose' (Array.length row) rows
  Nothing -> []
  where
    transpose' n = Array.replicate n [] # Array.foldl \cols row ->
      Array.zipWith (<>) cols (row <#> Array.singleton)

scrollIntoView :: String -> Effect Unit
scrollIntoView = runEffectFn1 scrollIntoView_

truthy :: forall a. a -> Boolean
truthy = runFn1 truthy_

foreign import scrollIntoView_ :: EffectFn1 String Unit

foreign import truthy_ :: forall a. Fn1 a Boolean

foreign import infinity :: Int
