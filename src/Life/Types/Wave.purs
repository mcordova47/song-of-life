module Life.Types.Wave
  ( Wave(..)
  , all
  , codec
  , default
  , display
  , icon
  , toJs
  )
  where

import Prelude

import Data.Bounded.Generic (genericBottom)
import Data.Codec as C
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Elmish (ReactElement)
import Life.Icons as I
import Life.Types.Codec (Codec)
import Life.Utils (allEnumValues)

data Wave
  = Triangle
  | Sine
  | Square
  | Sawtooth
derive instance Eq Wave
derive instance Ord Wave
derive instance Generic Wave _

codec ∷ Codec String Wave
codec = C.codec decode encode
  where
    encode = case _ of
      Triangle -> "W"
      Sine -> "S"
      Square -> "E"
      Sawtooth -> "Z"

    decode s
      | s == "S" = Just Sine
      | s == "W" = Just Triangle
      | s == "Z" = Just Sawtooth
      | s == "E" = Just Square
      | otherwise = Nothing

all :: Array Wave
all = allEnumValues

default :: Wave
default = genericBottom

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
