module Life.Types.Music.Note
  ( (\\)
  , Note(..)
  , a4
  , codec
  , dec
  , diff
  , display
  , drone
  , frequency
  , halfSteps
  , inc
  , play
  , schedule
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
import Effect.Uncurried (EffectFn2, EffectFn3, EffectFn4, runEffectFn2, runEffectFn3, runEffectFn4)
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

-- | A note is a pitch class combined with a certain octave. E.g. A♭3
data Note = Note PitchClass Int
derive instance Eq Note

instance Ord Note where
  compare = comparing halfSteps

instance Show Note where
  show (p \\ o) = PitchClass.display p <> show o

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

schedule :: Milliseconds -> Int -> Wave -> Note -> Effect Unit
schedule dur at wave note =
  runEffectFn4 schedule_ dur at (Wave.toJs wave) (frequency note)

drone :: Wave -> Note -> Effect { stop :: Effect Unit }
drone wave note =
  runEffectFn2 drone_ (Wave.toJs wave) (frequency note)

foreign import play_ :: EffectFn3 Milliseconds String Number Unit

foreign import schedule_ :: EffectFn4 Milliseconds Int String Number Unit

foreign import drone_ :: EffectFn2 String Number { stop :: Effect Unit }
