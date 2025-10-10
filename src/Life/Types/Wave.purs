module Life.Types.Wave
  ( Wave(..)
  , all
  , codec
  , decode
  , default
  , display
  , encode
  , icon
  , sawtooth
  , sine
  , square
  , triangle
  )
  where

import Prelude

import Data.Codec as C
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String as String
import Elmish (ReactElement)
import Elmish.HTML.Styled as H
import Life.Icons as I
import Life.Types.Codec (Codec)

newtype Wave = Wave String
derive instance Newtype Wave _
derive newtype instance Eq Wave

codec ∷ Codec String Wave
codec = C.codec decode encode

encode :: Wave -> String
encode wave
  | wave == sine = "S"
  | wave == triangle = "W"
  | wave == sawtooth = "Z"
  | wave == square = "E"
  | otherwise = ""

decode :: String -> Maybe Wave
decode s
  | s == "S" = Just sine
  | s == "W" = Just triangle
  | s == "Z" = Just sawtooth
  | s == "E" = Just square
  | otherwise = Nothing

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
