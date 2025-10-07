module Life.Wave
  ( Wave(..)
  , all
  , display
  , icon
  , sawtooth
  , sine
  , square
  , triangle
  )
  where

import Prelude

import Data.Newtype (class Newtype, unwrap)
import Data.String as String
import Elmish (ReactElement)
import Elmish.HTML.Styled as H
import Life.Icons as I

newtype Wave = Wave String
derive instance Newtype Wave _
derive newtype instance Eq Wave

all :: Array Wave
all =
  [ sine
  , square
  , triangle
  , sawtooth
  ]

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
