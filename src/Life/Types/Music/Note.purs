module Life.Types.Music.Note
  ( (\\)
  , Note(..)
  , a4
  , codec
  , dec
  , diff
  , display
  , frequency
  , halfSteps
  , inc
  , play
  )
  where

import Prelude hiding (degree)

import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Int as Int
import Data.Maybe (maybe)
import Data.Number (pow)
import Data.Profunctor (dimap)
import Data.Time.Duration (Milliseconds)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Elmish (ReactElement)
import Elmish.HTML.Styled as H
import Life.Types.Codec (Codec)
import Life.Types.Codec as Codec
import Life.Types.Music.Letter (Letter(..))
import Life.Types.Music.Modifier (natural)
import Life.Types.Music.PitchClass (PitchClass, (//))
import Life.Types.Music.PitchClass as PitchClass
import Life.Types.Music.Wave (Wave)
import Life.Types.Music.Wave as Wave

data Note = Note PitchClass Int
derive instance Eq Note

infixr 6 Note as \\

codec :: Codec String Note
codec =
  dimap toTuple fromTuple $
    Codec.rjoin "_" PitchClass.codec Codec.int
  where
    toTuple (p \\ o) = (p /\ o)
    fromTuple (p /\ o) = (p \\ o)

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
