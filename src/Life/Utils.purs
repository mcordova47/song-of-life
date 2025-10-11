module Life.Utils
  ( tags
  )
  where

import Prelude

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
