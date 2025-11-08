module Life.Types.Music.ScaleType
  ( ScaleType(..)
  , all
  , codec
  , default
  , display
  , notes
  , notes'
  , random
  , toScale
  )
  where

import Prelude

import Data.Array as A
import Data.Generic.Rep (class Generic)
import Effect (Effect)
import Life.Types.Codec (class Serializable, Codec)
import Life.Types.Codec as Codec
import Life.Types.Music.Note (Note)
import Life.Types.Music.PitchClass (PitchClass)
import Life.Types.Music.Scale (Scale)
import Life.Types.Music.Scale as Scale
import Life.Types.Music.Voicing as V
import Life.Utils.Generic as G

-- | A set of known scale types, which can be turned into Scales.
data ScaleType
  = Diatonic
  | Hexatonic
  | Pentatonic
derive instance Generic ScaleType _

instance Serializable ScaleType where
  codec = codec

codec :: Codec String ScaleType
codec = Codec.enum case _ of
  Diatonic -> "7"
  Hexatonic -> "6"
  Pentatonic -> "5"

all :: Array ScaleType
all = G.tags

random :: Effect ScaleType
random = G.randomTag

default :: ScaleType
default = Hexatonic

display :: ScaleType -> String
display = case _ of
  Diatonic -> "Diatonic"
  Hexatonic -> "Hexatonic"
  Pentatonic -> "Pentatonic"

toScale :: ScaleType -> Scale
toScale = case _ of
  Diatonic -> Scale.diatonic
  Hexatonic -> Scale.hexatonic
  Pentatonic -> Scale.pentatonic

notes :: forall r. Int -> { key :: PitchClass, notes :: Int, root :: Int, scale :: ScaleType | r } -> Array Note
notes = notes' V.default

notes' :: forall r. V.Voicing -> Int -> { key :: PitchClass, notes :: Int, root :: Int, scale :: ScaleType | r } -> Array Note
notes' voicing octave args =
  (Scale.shift args.root $ toScale args.scale).notes
    { key: args.key
    , root: octave
    , length: args.notes
    }
    # A.mapWithIndex voicing
