module Life.Types.Music.NamedDuration
  ( NamedDuration(..)
  , display
  , toDuration
  , toSeconds
  , toTime
  )
  where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..), Minutes(..), Seconds)
import Data.Time.Duration as D
import Life.Types.Codec (class Serializable)
import Life.Types.Codec as Codec
import Life.Types.Music.Duration (Duration(..))
import Life.Types.Music.Duration as MD

-- | A selection of known durations of notes which can be turned into fractional
-- | Durations.
data NamedDuration
  = Whole
  | Half
  | Quarter
  | Eighth
  | Sixteenth
  | ThirtySecond
  | SixtyFourth
derive instance Eq NamedDuration
derive instance Generic NamedDuration _

instance Serializable NamedDuration where
  codec = Codec.enum case _ of
    Whole -> "1"
    Half -> "2"
    Quarter -> "4"
    Eighth -> "8"
    Sixteenth -> "16"
    ThirtySecond -> "32"
    SixtyFourth -> "64"

display :: NamedDuration -> String
display = case _ of
  Whole -> "Whole"
  Half -> "Half"
  Quarter -> "Quarter"
  Eighth -> "Eighth"
  Sixteenth -> "Sixteenth"
  ThirtySecond -> "Thirty-Second"
  SixtyFourth -> "Sixty-Fourth"

toDuration :: NamedDuration -> Duration
toDuration = case _ of
  Whole -> Duration 1 1
  Half -> Duration 1 2
  Quarter -> Duration 1 4
  Eighth -> Duration 1 8
  Sixteenth -> Duration 1 16
  ThirtySecond -> Duration 1 32
  SixtyFourth -> Duration 1 64

toSeconds :: Int -> NamedDuration -> Number
toSeconds bpm =
  toTime@Seconds (Int.toNumber bpm) >>> unwrap

toTime :: forall @d. D.Duration d => Number -> NamedDuration -> d
toTime bpm =
  toDuration >>>
  MD.toNumber >>>
  (*) (measureDuration # D.fromDuration # unwrap) >>>
  Milliseconds >>>
  D.toDuration
  where
    measureDuration =
      Minutes (4.0 / bpm)
