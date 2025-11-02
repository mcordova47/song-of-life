module Life.Utils.Generic
  ( decodeTag
  , randomTag
  , tags
  )
  where

import Prelude

import Data.Bounded.Generic (class GenericBottom, class GenericTop, genericBottom, genericTop)
import Data.Enum.Generic (class GenericBoundedEnum, class GenericEnum, genericFromEnum, genericSucc, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable1 (class Unfoldable1, unfoldr1)
import Effect (Effect)
import Effect.Random as R
import Foreign.Object as O

tags :: forall @a @f rep
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

decodeTag :: forall @a rep
  . Generic a rep
  => GenericBottom rep
  => GenericEnum rep
  => GenericBoundedEnum rep
  => (a -> String)
  -> String
  -> Maybe a
decodeTag encode s =
    O.lookup s mapping
  where
    mapping = tags @a @Array <#> (\t -> encode t /\ t) # O.fromFoldable
