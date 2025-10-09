-- TODO: Move under Life.Types
module Life.Wave
  ( Wave(..)
  , all
  , codec
  , default
  , display
  , icon
  , sawtooth
  , sine
  , square
  , triangle
  )
  where

import Prelude

import Data.Argonaut (Json)
import Data.Codec.Argonaut (Codec, JsonDecodeError)
import Data.Codec.Argonaut as C
import Data.Either (Either)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (wrapIso)
import Data.String as String
import Elmish (ReactElement)
import Elmish.HTML.Styled as H
import Life.Icons as I

newtype Wave = Wave String
derive instance Newtype Wave _
derive newtype instance Eq Wave

codec ∷ Codec (Either JsonDecodeError) Json Json Wave Wave
codec = wrapIso Wave C.string

all :: Array Wave
all =
  [ triangle
  , sine
  , square
  , sawtooth
  ]

default :: Wave
default = triangle

sine :: Wave
sine = Wave "sine"

square :: Wave
square = Wave "square"

triangle :: Wave
triangle = Wave "triangle"

sawtooth :: Wave
sawtooth = Wave "sawtooth"

display :: Wave -> String
display =
  unwrap
  >>> String.splitAt 1
  >>> \{ before, after } -> String.toUpper before <> after

icon :: I.Props -> Wave -> ReactElement
icon props wave
  | wave == sine = I.sineWave props
  | wave == square = I.squareWave props
  | wave == triangle = I.triangleWave props
  | wave == sawtooth = I.sawtoothWave props
  | otherwise = H.empty
