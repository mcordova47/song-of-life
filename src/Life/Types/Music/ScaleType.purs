module Life.Types.Music.ScaleType
  ( ScaleType(..)
  , all
  , codec
  , default
  , display
  , random
  , toScale
  )
  where

import Prelude

import Data.Codec as C
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Life.Types.Codec (class Serializable, Codec)
import Life.Types.Music.Scale (Scale)
import Life.Types.Music.Scale as Scale
import Life.Utils as U

data ScaleType
  = Diatonic
  | Hexatonic
  | Pentatonic
derive instance Generic ScaleType _

instance Serializable ScaleType where
  codec = codec

codec :: Codec String ScaleType
codec = C.codec decode encode
  where
    decode str
      | str == "7" = Just Diatonic
      | str == "6" = Just Hexatonic
      | str == "5" = Just Pentatonic
      | otherwise = Nothing

    encode = case _ of
      Diatonic -> "7"
      Hexatonic -> "6"
      Pentatonic -> "5"

all :: Array ScaleType
all = U.tags

random :: Effect ScaleType
random = U.randomTag

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
