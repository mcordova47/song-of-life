module Life.Types.Game.Rule
  ( Rule
  , default
  , fromNamed
  )
  where

import Prelude

import Data.Array (elem)
import Data.Codec as C
import Life.Types.Game.NamedRule (NamedRule)
import Life.Types.Game.NamedRule as N
import Life.Types.Game.RuleDescriptor (B(..), RuleDescriptor, S(..), (|/|))

-- | The most general representation of a rule, which is a function that accepts
-- | a Boolean (is the cell alive) and an Int (how many of its neighbors are
-- | alive) and returns a Boolean indicating whether it will be alive on the
-- | next step.
type Rule = Boolean -> Int -> Boolean

default :: Rule
default = fromNamed N.default

fromDescriptor :: RuleDescriptor -> Rule
fromDescriptor ((B birth) |/| (S survival)) living neighbors
  | living && neighbors `elem` survival = true
  | not living && neighbors `elem` birth = true
  | otherwise = false

fromNamed :: NamedRule -> Rule
fromNamed = C.encode N.descriptorCodec >>> fromDescriptor
