module Life.Types.Music.Note
  ( (\\)
  , Interval
  , Note(..)
  , a4
  , addDiatonic
  , dec
  , diff
  , display
  , fifth
  , fourth
  , frequency
  , halfSteps
  , inc
  , octave
  , play
  , second
  , seventh
  , sixth
  , third
  , tonal
  )
  where

import Prelude

import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Int as Int
import Data.Maybe (maybe)
import Data.Number (pow)
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Elmish (ReactElement)
import Elmish.HTML.Styled as H
import Life.Types.Musc.Letter (Letter(..))
import Life.Types.Musc.Modifier (natural)
import Life.Types.Musc.PitchClass (PitchClass, (//))
import Life.Types.Musc.PitchClass as PitchClass
import Life.Types.Music.Wave (Wave)
import Life.Types.Music.Wave as Wave

data Note = Note PitchClass Int
derive instance Eq Note

infixr 6 Note as \\

type Interval = PitchClass -> Note -> Note

display :: Note -> ReactElement
display (p \\ o) = H.fragment
  [ H.text $ PitchClass.display p
  , H.sub "" $ show o
  ]

a4 :: Note
a4 = A // natural \\ 4

-- Steps above C0
halfSteps :: Note -> Int
halfSteps (pitchClass \\ octave') =
  PitchClass.halfSteps pitchClass + 12 * octave'

diff :: Note -> Note -> Int
diff a b =
  halfSteps a - halfSteps b

addDiatonic :: Int -> Interval
addDiatonic degrees key note@(pitchClass \\ octave')
  | degrees > 0 = addDiatonic (degrees - 1) key (inc note)
  | degrees < 0 = addDiatonic (degrees + 1) key (dec note)
  | otherwise = PitchClass.fix key pitchClass \\ octave'

tonal :: Interval
tonal = addDiatonic 0

second :: Interval
second = addDiatonic 1

third :: Interval
third = addDiatonic 2

fourth :: Interval
fourth = addDiatonic 3

fifth :: Interval
fifth = addDiatonic 4

sixth :: Interval
sixth = addDiatonic 5

seventh :: Interval
seventh = addDiatonic 6

octave :: Interval
octave = addDiatonic 7

inc :: Note -> Note
inc (l // m \\ o) = l
  # genericSucc
  # maybe (genericBottom // m \\ (o + 1)) \l' -> (l' // m \\ o)

dec :: Note -> Note
dec (l // m \\ o) = l
  # genericPred
  # maybe (genericTop // m \\ (o - 1)) \l' -> (l' // m \\ o)

frequency :: Note -> Number
frequency note = multiplier * 440.0
  where
    multiplier = pow halfStep (Int.toNumber diff')
    halfStep = pow 2.0 (1.0 / 12.0)
    diff' = diff note a4

play :: Milliseconds -> Wave -> Note -> Effect Unit
play dur wave note =
  runEffectFn3 play_ dur (Wave.toJs wave) (frequency note)

foreign import play_ :: EffectFn3 Milliseconds String Number Unit
