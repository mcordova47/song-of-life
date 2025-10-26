module Life.Types.NamedRule
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
import Life.Types.RuleDescriptor (RuleDescriptor)
import Life.Types.RuleDescriptor as D

data NamedRule
  = Life
  | HighLife
  | Seeds
  | LifeWithoutDeath
  | DayAndNight
  | Morley
  | Replicator
  | Maze
  | MazeWithMice
derive instance Generic NamedRule _

instance Serializable NamedRule where
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
        "Maze" -> Just Maze
        "MazeWithMice" -> Just MazeWithMice
        _ -> Nothing

      encode = case _ of
        Life -> "Life"
        HighLife -> "HighLife"
        Seeds -> "Seeds"
        LifeWithoutDeath -> "LifeWithoutDeath"
        DayAndNight -> "DayAndNight"
        Morley -> "Morley"
        Replicator -> "Replicator"
        Maze -> "Maze"
        MazeWithMice -> "MazeWithMice"

descriptorCodec :: Codec RuleDescriptor NamedRule
descriptorCodec = C.codec decode encode
  where
    decode descriptor
      | descriptor == D.life = Just Life
      | descriptor == D.highLife = Just HighLife
      | descriptor == D.seeds = Just Seeds
      | descriptor == D.lifeWithoutDeath = Just LifeWithoutDeath
      | descriptor == D.dayAndNight = Just DayAndNight
      | descriptor == D.morley = Just Morley
      | descriptor == D.replicator = Just Replicator
      | descriptor == D.maze = Just Maze
      | descriptor == D.mazeWithMice = Just MazeWithMice
      | otherwise = Nothing

    encode = case _ of
      Life -> D.life
      HighLife -> D.highLife
      Seeds -> D.seeds
      LifeWithoutDeath -> D.lifeWithoutDeath
      DayAndNight -> D.dayAndNight
      Morley -> D.morley
      Replicator -> D.replicator
      Maze -> D.maze
      MazeWithMice -> D.mazeWithMice

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
  Seeds -> "Seeds"
  LifeWithoutDeath -> "Life Without Death"
  DayAndNight -> "Day And Night"
  Morley -> "Morley"
  Replicator -> "Replicator"
  Maze -> "Maze"
  MazeWithMice -> "Maze with Mice"
