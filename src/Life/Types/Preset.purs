module Life.Types.Preset
  ( Preset(..)
  , PresetV0
  , PresetV1
  , all
  , bottomRightGlider
  , codec
  , collision
  , default
  , fromCells
  , fromState
  , glider
  , heart
  , livingCells
  , musicNotes
  , octocat
  , pond
  , wave
  )
  where

import Prelude

import Control.Alt ((<|>))
import Data.Codec as C
import Data.Profunctor (dimap)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested (type (/\), (/\))
import Life.Types.Cell (Cell)
import Life.Types.Codec (Codec, (/>), (</>))
import Life.Types.Codec as Codec
import Life.Types.Grid as Grid
import Life.Types.Grid.Compressed as GridCompressed
import Life.Types.Wave (Wave)
import Life.Types.Wave as Wave

data Preset
  = V0 PresetV0
  | V1 PresetV1

type PresetV0 =
  { livingCells :: Set Cell
  , wave :: Wave
  }

type PresetV1 = PresetV0

codec ∷ Codec String Preset
codec = C.codec decode encode
  where
    decode s = V0 <$> C.decode codecV0 s <|> V1 <$> C.decode codecV1 s

    encode = case _ of
      V0 p -> C.encode codecV0 p
      V1 p -> C.encode codecV1 p

    codecV0 = Codec.literal "0" /> codecV0' (Grid.cellsCodec </> Wave.codec)

    codecV1 = Codec.literal "1" /> codecV0' (GridCompressed.cellsCodec </> Wave.codec)

codecV0' :: Codec String (Set Cell /\ Wave) -> Codec String PresetV1
codecV0' = dimap toTuple fromTuple
  where
    fromTuple (g /\ w) = { livingCells: g, wave: w }
    toTuple p = p.livingCells /\ p.wave

fromCells :: Set Cell -> Preset
fromCells = V1 <<< { livingCells: _, wave: Wave.default }

fromState :: forall r. { livingCells :: Set Cell, wave :: Wave | r } -> Preset
fromState s = V1 { livingCells: s.livingCells, wave: s.wave }

livingCells :: Preset -> Set Cell
livingCells = case _ of
  V0 p -> p.livingCells
  V1 p -> p.livingCells

wave :: Preset -> Wave
wave = case _ of
  V0 p -> p.wave
  V1 p -> p.wave

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