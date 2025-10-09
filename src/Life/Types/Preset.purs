module Life.Types.Preset
  ( Preset(..)
  , PresetV1
  , all
  , bottomRightGlider
  , codec
  , collision
  , decode
  , default
  , encode
  , encodeJson
  , fromCells
  , fromState
  , glider
  , heart
  , livingCells
  , musicNotes
  , octocat
  , pond
  , urlCodec
  , wave
  )
  where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut as J
import Data.Codec.Argonaut (Codec, JsonDecodeError, (>~>))
import Data.Codec.Argonaut as C
import Data.Codec.Argonaut.Common as CC
import Data.Codec.Argonaut.Record as CR
import Data.Codec.Argonaut.Variant as CAV
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe)
import Data.Profunctor (dimap)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Data.Variant as V
import Life.Compression as Compression
import Life.Json as Json
import Life.Types.Cell (Cell)
import Life.Types.Cell as Cell
import Life.Wave (Wave)
import Life.Wave as Wave
import Type.Proxy (Proxy(..))

data Preset
  = V1 PresetV1

type PresetV1 =
  { livingCells :: Set Cell
  , wave :: Wave
  }

urlCodec ∷ Codec (Either JsonDecodeError) String String Preset Preset
urlCodec = Compression.codec >~> Json.codec >~> codec

codec :: Codec (Either JsonDecodeError) Json Json Preset Preset
codec =
  dimap toVariant fromVariant $ CAV.variantMatch
    { v1: Right codecv1 }
  where
    toVariant = case _ of
      V1 p -> V.inj (Proxy :: _ "v1") p
    fromVariant = V.match
      { v1: V1 }

codecv1 :: Codec (Either JsonDecodeError) Json Json PresetV1 PresetV1
codecv1 =
  C.object "PresetV1" $ CR.record
    { livingCells: CC.set Cell.codec
    , wave: Wave.codec
    }

encode :: Preset -> String
encode = C.encode urlCodec

decode :: String -> Maybe Preset
decode = C.decode urlCodec >>> hush

encodeJson :: Preset -> String
encodeJson = C.encode codec >>> J.stringify

fromCells :: Set Cell -> Preset
fromCells = V1 <<< { livingCells: _, wave: Wave.default }

fromState :: forall r. { livingCells :: Set Cell, wave :: Wave | r } -> Preset
fromState s = V1 { livingCells: s.livingCells, wave: s.wave }

livingCells :: Preset -> Set Cell
livingCells (V1 p) = p.livingCells

wave :: Preset -> Wave
wave (V1 p) = p.wave

presetV1 :: Array Cell -> Preset
presetV1 = fromCells <<< Set.fromFoldable

default :: Preset
default = heart

all :: Array Preset
all = [heart, pond, octocat, musicNotes, glider, collision]

heart :: Preset
heart = presetV1
  [ 0 /\ 3
  , 0 /\ 4
  , 0 /\ 5
  , 0 /\ 9
  , 0 /\ 10
  , 0 /\ 11
  , 1 /\ 2
  , 1 /\ 6
  , 1 /\ 8
  , 1 /\ 12
  , 2 /\ 1
  , 2 /\ 7
  , 2 /\ 13
  , 3 /\ 1
  , 3 /\ 13
  , 4 /\ 1
  , 4 /\ 2
  , 4 /\ 13
  , 5 /\ 2
  , 5 /\ 3
  , 5 /\ 12
  , 6 /\ 3
  , 6 /\ 4
  , 6 /\ 11
  , 7 /\ 4
  , 7 /\ 5
  , 7 /\ 10
  , 8 /\ 5
  , 8 /\ 6
  , 8 /\ 9
  , 9 /\ 6
  , 9 /\ 7
  , 9 /\ 8
  , 10 /\ 7
  ]

glider :: Preset
glider = presetV1 gliderCells

gliderCells :: Array Cell
gliderCells =
  [ 0 /\ 2
  , 1 /\ 0
  , 1 /\ 2
  , 2 /\ 1
  , 2 /\ 2
  ]

bottomRightGlider :: Preset
bottomRightGlider = presetV1 bottomRightGliderCells

bottomRightGliderCells :: Array Cell
bottomRightGliderCells =
  [ 11 /\ 13
  , 10 /\ 15
  , 10 /\ 13
  , 9 /\ 14
  , 9 /\ 13
  ]

collision :: Preset
collision = presetV1 $ gliderCells <> bottomRightGliderCells

octocat :: Preset
octocat = presetV1
  [ 1 /\ 5
  , 1 /\ 6
  , 1 /\ 10
  , 1 /\ 11
  , 2 /\ 5
  , 2 /\ 7
  , 2 /\ 8
  , 2 /\ 9
  , 2 /\ 11
  , 3 /\ 4
  , 3 /\ 12
  , 4 /\ 4
  , 4 /\ 12
  , 5 /\ 4
  , 5 /\ 12
  , 6 /\ 5
  , 6 /\ 11
  , 7 /\ 3
  , 7 /\ 6
  , 7 /\ 10
  , 8 /\ 4
  , 8 /\ 5
  , 8 /\ 7
  , 8 /\ 9
  , 9 /\ 6
  , 9 /\ 10
  , 10 /\ 6
  , 10 /\ 10
  ]

musicNotes :: Preset
musicNotes = presetV1
  [ 1 /\ 9
  , 1 /\ 10
  , 2 /\ 7
  , 2 /\ 8
  , 2 /\ 10
  , 3 /\ 5
  , 3 /\ 6
  , 3 /\ 10
  , 4 /\ 3
  , 4 /\ 4
  , 4 /\ 6
  , 4 /\ 10
  , 4 /\ 11
  , 4 /\ 12
  , 5 /\ 6
  , 5 /\ 10
  , 5 /\ 13
  , 6 /\ 6
  , 6 /\ 7
  , 6 /\ 8
  , 6 /\ 11
  , 6 /\ 12
  , 7 /\ 6
  , 7 /\ 9
  , 8 /\ 7
  , 8 /\ 8
  ]

pond :: Preset
pond = presetV1
  [ 0 /\ 9
  , 0 /\ 10
  , 1 /\ 3
  , 1 /\ 4
  , 1 /\ 5
  , 1 /\ 8
  , 1 /\ 11
  , 2 /\ 2
  , 2 /\ 3
  , 2 /\ 4
  , 2 /\ 9
  , 2 /\ 10
  , 3 /\ 13
  , 3 /\ 14
  , 4 /\ 12
  , 4 /\ 15
  , 5 /\ 13
  , 5 /\ 14
  , 6 /\ 5
  , 6 /\ 6
  , 7 /\ 4
  , 7 /\ 7
  , 8 /\ 5
  , 8 /\ 6
  , 9 /\ 1
  , 9 /\ 2
  , 9 /\ 11
  , 9 /\ 12
  , 9 /\ 13
  , 10 /\ 0
  , 10 /\ 3
  , 10 /\ 10
  , 10 /\ 11
  , 10 /\ 12
  , 11 /\ 1
  , 11 /\ 2
  ]