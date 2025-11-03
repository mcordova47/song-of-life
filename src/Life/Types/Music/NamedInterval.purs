module Life.Types.Music.NamedInterval
  ( MajMin(..)
  , NamedInterval(..)
  , Perf(..)
  , majMinModifier
  , perfModifier
  , toInterval
  )
  where

import Prelude

import Life.Types.Music.Degree as D
import Life.Types.Music.Interval (Interval(..))
import Life.Types.Music.IntervalModifier (IntervalModifier)
import Life.Types.Music.IntervalModifier as M

data NamedInterval
  = Unison
  | Second MajMin
  | Third MajMin
  | Fourth Perf
  | Fifth Perf
  | Sixth MajMin
  | Seventh MajMin
  | Ninth MajMin

data MajMin
  = Minor
  | Major

data Perf
  = Diminished
  | Perfect
  | Augmented

majMinModifier :: MajMin -> IntervalModifier
majMinModifier = case _ of
  Minor -> M.minor
  Major -> M.major

perfModifier :: Perf -> IntervalModifier
perfModifier = case _ of
  Diminished -> M.diminished
  Perfect -> M.perfect
  Augmented -> M.augmented

toInterval :: NamedInterval -> Interval
toInterval = case _ of
  Unison -> Interval D.tonal M.none
  Second m -> Interval D.second $ majMinModifier m
  Third m -> Interval D.third $ majMinModifier m
  Fourth m -> Interval D.fourth $ perfModifier m
  Fifth m -> Interval D.fifth $ perfModifier m
  Sixth m -> Interval D.sixth $ majMinModifier m
  Seventh m -> Interval D.seventh $ majMinModifier m
  Ninth m -> Interval (D.degree 9) $ majMinModifier m
