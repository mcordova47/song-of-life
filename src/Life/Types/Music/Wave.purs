module Life.Types.Music.Wave
  ( Wave(..)
  , all
  , codec
  , default
  , display
  , icon
  , random
  , toJs
  )
  where

import Prelude

import Data.Bounded.Generic (genericBottom)
import Data.Generic.Rep (class Generic)
import Effect (Effect)
import Elmish (ReactElement)
import Life.Components.Icons as I
import Life.Types.Codec (class Serializable, Codec)
import Life.Types.Codec as Codec
import Life.Utils.Generic as G

data Wave
  = Triangle
  | Sine
  | Square
  | Sawtooth
derive instance Eq Wave
derive instance Ord Wave
derive instance Generic Wave _

instance Serializable Wave where
  codec = codec

codec ∷ Codec String Wave
codec = Codec.enum case _ of
  Triangle -> "W"
  Sine -> "S"
  Square -> "E"
  Sawtooth -> "Z"

all :: Array Wave
all = G.tags

default :: Wave
default = genericBottom

random :: Effect Wave
random = G.randomTag

display :: Wave -> String
display = case _ of
  Triangle -> "Triangle"
  Sine -> "Sine"
  Square -> "Square"
  Sawtooth -> "Sawtooth"

toJs :: Wave -> String
toJs = case _ of
  Triangle -> "triangle"
  Sine -> "sine"
  Square -> "square"
  Sawtooth -> "sawtooth"

icon :: I.Props -> Wave -> ReactElement
icon props = case _ of
  Triangle -> I.triangleWave props
  Sine -> I.sineWave props
  Square -> I.squareWave props
  Sawtooth -> I.sawtoothWave props
