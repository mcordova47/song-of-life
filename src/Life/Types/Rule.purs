module Life.Types.Rule
  ( Rule
  , default
  , fromNamed
  )
  where

import Prelude

import Data.Array (elem)
import Life.Types.RuleDescriptor (B(..), RuleDescriptor, S(..), (|/|))
import Life.Types.RuleDescriptor as D
import Life.Types.NamedRule (NamedRule(..))
import Life.Types.NamedRule as N

type Rule = Boolean -> Int -> Boolean

default :: Rule
default = fromNamed N.default

fromDescriptor :: RuleDescriptor -> Rule
fromDescriptor ((B birth) |/| (S survival)) living neighbors
  | living && neighbors `elem` survival = true
  | not living && neighbors `elem` birth = true
  | otherwise = false

fromNamed :: NamedRule -> Rule
fromNamed = case _ of
  Life -> fromDescriptor D.life
  HighLife -> fromDescriptor D.highLife
  Seeds -> fromDescriptor D.seeds
  LifeWithoutDeath -> fromDescriptor D.lifeWithoutDeath
  DayAndNight -> fromDescriptor D.dayAndNight
  Morley -> fromDescriptor D.morley
  Replicator -> fromDescriptor D.replicator
