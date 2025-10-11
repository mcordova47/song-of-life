module Life.Utils
  ( allEnumValues
  )
  where

import Prelude

import Data.Bounded.Generic (class GenericBottom, genericBottom)
import Data.Enum.Generic (class GenericBoundedEnum, class GenericEnum, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (unfoldr)

allEnumValues :: forall a rep
  . Generic a rep
  => GenericBottom rep
  => GenericEnum rep
  => GenericBoundedEnum rep
  => Array a
allEnumValues = genericBottom # unfoldr \w -> traverse genericSucc (w /\ w)
