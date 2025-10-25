module Life.Types.Rule
  ( Rule
  , RuleType(..)
  , default
  , defaultType
  , display
  , rule
  )
  where

import Prelude

import Data.Array (elem, (..))
import Data.Codec as C
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Life.Types.Codec (class Serializable)

type Rule = Boolean -> Int -> Boolean

data RuleType
  = Life
  | HighLife
  | Seeds
  | LifeWithoutDeath
  | DayAndNight
  | Morley
  | Replicator
derive instance Generic RuleType _

instance Serializable RuleType where
  codec = C.codec decode encode
    where
      decode = case _ of
        "Life" -> Just Life
        "HighLife" -> Just HighLife
        "Seeds" -> Just Seeds
        "LifeWithoutDeath" -> Just LifeWithoutDeath
        "DayAndNight" -> Just DayAndNight
        "Morley" -> Just Morley
        "Replicator" -> Just Replicator
        _ -> Nothing

      encode = case _ of
        Life -> "Life"
        HighLife -> "HighLife"
        Seeds -> "Seeds"
        LifeWithoutDeath -> "LifeWithoutDeath"
        DayAndNight -> "DayAndNight"
        Morley -> "Morley"
        Replicator -> "Replicator"

newtype B = B (Array Int)
newtype S = S (Array Int)

shorthand :: B -> S -> Rule
shorthand (B birth) (S survival) living neighbors
  | living && neighbors `elem` survival = true
  | not living && neighbors `elem` birth = true
  | otherwise = false

infixl 5 shorthand as |/|

default :: Rule
default = rule defaultType

defaultType :: RuleType
defaultType = Life

rule :: RuleType -> Rule
rule = case _ of
  Life -> life
  HighLife -> highLife
  Seeds -> seeds
  LifeWithoutDeath -> lifeWithoutDeath
  DayAndNight -> dayAndNight
  Morley -> morley
  Replicator -> replicator

display :: RuleType -> String
display = case _ of
  Life -> "Life (B3/S23)"
  HighLife -> "High Life (B36/S23)"
  Seeds -> "Seeds (B2/S)"
  LifeWithoutDeath -> "Life Without Death (B3/S012345678)"
  DayAndNight -> "Day And Night (B3678/S34678)"
  Morley -> "Morley (B368/S245)"
  Replicator -> "Replicator (B1357/S1357)"

life :: Rule
life = B [3] |/| S [2, 3]

highLife :: Rule
highLife = B [3, 6] |/| S [2, 3]

seeds :: Rule
seeds = B [2] |/| S []

lifeWithoutDeath :: Rule
lifeWithoutDeath = B [3] |/| S (0 .. 8)

dayAndNight :: Rule
dayAndNight = B [3, 6, 7, 8] |/| S [3, 4, 6, 7, 8]

morley :: Rule
morley = B [3, 6, 8] |/| S [2, 4, 5]

replicator :: Rule
replicator = B [1, 3, 5, 7] |/| S [1, 3, 5, 7]
