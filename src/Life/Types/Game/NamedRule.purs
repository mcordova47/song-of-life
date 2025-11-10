module Life.Types.Game.NamedRule
  ( NamedRule(..)
  , default
  , descriptorCodec
  , display
  , name
  )
  where

import Prelude

import Data.Codec as C
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Life.Types.Codec (class Serializable, Codec)
import Life.Types.Codec as Codec
import Life.Types.Game.RuleDescriptor (RuleDescriptor)
import Life.Types.Game.RuleDescriptor as D

-- | The most constrained type of rule — a known rule which is named. These are
-- | mapped to rule descriptors and rules.
data NamedRule
  = Life
  | HighLife
  | DryLife
  | Seeds
  | LifeWithoutDeath
  | DayAndNight
  | Morley
  | Replicator
  | Maze
  | MazeWithMice
  | Stains
  | DotLife
  | WalledCities
  | Gnarl
  | Diamoeba
derive instance Eq NamedRule
derive instance Generic NamedRule _

instance Serializable NamedRule where
  codec = Codec.enum case _ of
    Life -> "Life"
    HighLife -> "HighLife"
    DryLife -> "DryLife"
    Seeds -> "Seeds"
    LifeWithoutDeath -> "LifeWithoutDeath"
    DayAndNight -> "DayAndNight"
    Morley -> "Morley"
    Replicator -> "Replicator"
    Maze -> "Maze"
    MazeWithMice -> "MazeWithMice"
    Stains -> "Stains"
    DotLife -> "DotLife"
    WalledCities -> "WalledCities"
    Gnarl -> "Gnarl"
    Diamoeba -> "Diamoeba"

descriptorCodec :: Codec RuleDescriptor NamedRule
descriptorCodec = C.codec decode encode
  where
    decode descriptor
      | descriptor == D.life = Just Life
      | descriptor == D.highLife = Just HighLife
      | descriptor == D.dryLife = Just DryLife
      | descriptor == D.seeds = Just Seeds
      | descriptor == D.lifeWithoutDeath = Just LifeWithoutDeath
      | descriptor == D.dayAndNight = Just DayAndNight
      | descriptor == D.morley = Just Morley
      | descriptor == D.replicator = Just Replicator
      | descriptor == D.maze = Just Maze
      | descriptor == D.mazeWithMice = Just MazeWithMice
      | descriptor == D.stains = Just Stains
      | descriptor == D.dotLife = Just DotLife
      | descriptor == D.walledCities = Just WalledCities
      | descriptor == D.gnarl = Just Gnarl
      | otherwise = Nothing

    encode = case _ of
      Life -> D.life
      HighLife -> D.highLife
      DryLife -> D.dryLife
      Seeds -> D.seeds
      LifeWithoutDeath -> D.lifeWithoutDeath
      DayAndNight -> D.dayAndNight
      Morley -> D.morley
      Replicator -> D.replicator
      Maze -> D.maze
      MazeWithMice -> D.mazeWithMice
      Stains -> D.stains
      DotLife -> D.dotLife
      WalledCities -> D.walledCities
      Gnarl -> D.gnarl
      Diamoeba -> D.diamoeba

default :: NamedRule
default = Life

display :: NamedRule -> String
display ruleType =
  name ruleType <> " (" <> D.display descriptor <> ")"
  where
    descriptor = C.encode descriptorCodec ruleType

name :: NamedRule -> String
name = case _ of
  Life -> "Life"
  HighLife -> "High Life"
  DryLife -> "Dry Life"
  Seeds -> "Seeds"
  LifeWithoutDeath -> "Life Without Death"
  DayAndNight -> "Day And Night"
  Morley -> "Morley"
  Replicator -> "Replicator"
  Maze -> "Maze"
  MazeWithMice -> "Maze with Mice"
  Stains -> "Stains"
  DotLife -> "Dot Life"
  WalledCities -> "Walled Cities"
  Gnarl -> "Gnarl"
  Diamoeba -> "Diamoeba"
