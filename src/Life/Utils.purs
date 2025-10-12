module Life.Utils
  ( (>>>>)
  , chunksOf
  , compose2
  , fill
  , tags
  )
  where

import Prelude

import Data.Array as Array
import Data.Bounded.Generic (class GenericBottom, genericBottom)
import Data.Enum.Generic (class GenericBoundedEnum, class GenericEnum, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (class Unfoldable1, unfoldr1)

tags :: forall f a rep
  . Generic a rep
  => GenericBottom rep
  => GenericEnum rep
  => GenericBoundedEnum rep
  => Unfoldable1 f
  => f a
tags = genericBottom # unfoldr1 \w -> (w /\ genericSucc w)

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
