module Life.Types.Preset
  ( Preset(..)
  , PresetV0
  , PresetV0'
  , PresetV1
  , all
  , beatsPerMeasure
  , codec
  , default
  , defaultBeatsPerMeasure
  , defaultKey
  , defaultNotes
  , defaultOctave
  , empty
  , fromState
  , headphones
  , key
  , livingCells
  , notes
  , random
  , root
  , scale
  , toLife
  , wave
  )
  where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((!!))
import Data.Array as Array
import Data.Codec as C
import Data.Maybe (Maybe)
import Data.Profunctor (dimap)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Random as R
import Life.Types.Cell (Cell)
import Life.Types.Cell as Cell
import Life.Types.Codec (Codec, (/>), (</>), (<\>))
import Life.Types.Codec as Codec
import Life.Types.Grid as Grid
import Life.Types.Grid.Compressed as GridCompressed
import Life.Types.Life (class Life)
import Life.Types.Life as Life
import Life.Types.Music.Letter (Letter(..))
import Life.Types.Music.Modifier (flat, natural)
import Life.Types.Music.PitchClass (PitchClass, (//))
import Life.Types.Music.PitchClass as PitchClass
import Life.Types.Music.ScaleType (ScaleType)
import Life.Types.Music.ScaleType as ScaleType
import Life.Types.Music.Wave (Wave)
import Life.Types.Music.Wave as Wave

data Preset
  = V0 PresetV0
  | V1 PresetV1

type PresetV0' r =
  { beatsPerMeasure :: Int
  , key :: PitchClass
  , livingCells :: Set Cell
  , notes :: Int
  , root :: Int
  , scale :: ScaleType
  , wave :: Wave
  | r
  }

type PresetV0 = PresetV0' ()

type PresetV1 = PresetV0

codec ∷ Codec String Preset
codec = C.codec decode encode
  where
    decode s = V0 <$> C.decode codecV0 s <|> V1 <$> C.decode codecV1 s

    encode = case _ of
      V0 p -> C.encode codecV0 p
      V1 p -> C.encode codecV1 p

    codecV0 = codecV0' $ (Codec.literal "0" /> PitchClass.codec </> ScaleType.codec </> Codec.int </> Grid.cellsCodec) <\> Wave.codec

    codecV1 = codecV0' $ (Codec.literal "1" /> PitchClass.codec </> ScaleType.codec </> Codec.int </> GridCompressed.cellsCodec) <\> Wave.codec

codecV0' :: Codec String ((PitchClass /\ ScaleType /\ Int /\ Set Cell) /\ Wave) -> Codec String PresetV1
codecV0' = dimap toTuple fromTuple
  where
    fromTuple ((k /\ s /\ r /\ g) /\ w) =
      { beatsPerMeasure: defaultBeatsPerMeasure
      , key: k
      , livingCells: g
      , notes: defaultNotes
      , root: r
      , scale: s
      , wave: w
      }
    toTuple p = (p.key /\ p.scale /\ p.root /\ p.livingCells) /\ p.wave

fromCells :: Set Cell -> Preset
fromCells = fromState <<<
  { beatsPerMeasure: defaultBeatsPerMeasure
  , key: defaultKey
  , notes: defaultNotes
  , root: 0
  , scale: ScaleType.default
  , livingCells: _
  , wave: Wave.default
  }

fromState :: forall r. PresetV0' r -> Preset
fromState s = V1
  { beatsPerMeasure: s.beatsPerMeasure
  , key: s.key
  , livingCells: s.livingCells
  , notes: s.notes
  , root: s.root
  , scale: s.scale
  , wave: s.wave
  }

toLife :: forall @f. Life f => Preset -> f Boolean
toLife p = Life.fromCells (notes p) (beatsPerMeasure p) (livingCells p)

unwrap :: Preset -> PresetV0
unwrap = case _ of
  V0 p -> p
  V1 p -> p

key :: Preset -> PitchClass
key = unwrap >>> _.key

livingCells :: Preset -> Set Cell
livingCells = unwrap >>> _.livingCells

wave :: Preset -> Wave
wave = unwrap >>> _.wave

root :: Preset -> Int
root = unwrap >>> _.root

scale :: Preset -> ScaleType
scale = unwrap >>> _.scale

beatsPerMeasure :: Preset -> Int
beatsPerMeasure = unwrap >>> _.beatsPerMeasure

notes :: Preset -> Int
notes = unwrap >>> _.notes

presetV1 :: Array Cell -> Preset
presetV1 = fromCells <<< Set.fromFoldable

presetV1' ::
  { key :: PitchClass
  , root :: Int
  , scale :: ScaleType
  , wave :: Wave
  }
  -> Array Cell
  -> Preset
presetV1' p cells = fromState
  { beatsPerMeasure: defaultBeatsPerMeasure
  , key: p.key
  , livingCells: Set.fromFoldable cells
  , notes: defaultNotes
  , root: p.root
  , scale: p.scale
  , wave: p.wave
  }

random :: Int -> Int -> Effect (Maybe Preset)
random rows cols = do
  r <- R.randomInt (-6) 6
  cells <- Cell.randomSet rows cols
  w <- Wave.random
  s <- ScaleType.random
  keyIndex <- R.randomInt 0 (Array.length PitchClass.all - 1)
  let mKey = PitchClass.all !! keyIndex
  for mKey \k ->
    pure $ V1
      { beatsPerMeasure: defaultBeatsPerMeasure
      , key: k
      , livingCells: cells
      , notes: defaultNotes
      , root: r
      , scale: s
      , wave: w
      }

default :: Preset
default = galaxy

empty :: Preset
empty = presetV1 []

all :: Array (String /\ Preset)
all =
  [ "Galaxy" /\ galaxy
  , "Headphones" /\ headphones
  , "Flower" /\ flower
  , "Glider" /\ glider
  , "Collision" /\ collision
  , "Sky" /\ sky
  , "Spaceship" /\ spaceship
  , "Ships Passing" /\ shipsPassing
  , "Octocat" /\ octocat
  , "Heart" /\ heart
  , "Pulsar" /\ pulsar
  ]

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
glider = presetV1
  [ 0 /\ 2
  , 1 /\ 0
  , 1 /\ 2
  , 2 /\ 1
  , 2 /\ 2
  ]

collision :: Preset
collision = presetV1
  [ 2 /\ 1
  , 2 /\ 3
  , 3 /\ 2
  , 3 /\ 3
  , 4 /\ 2
  , 13 /\ 12
  , 13 /\ 13
  , 14 /\ 12
  , 14 /\ 14
  , 15 /\ 12
  ]

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

headphones :: Preset
headphones = presetV1'
  { key: A // flat
  , root: 0
  , scale: ScaleType.Hexatonic
  , wave: Wave.Square
  }
  [ 2 /\ 5
  , 2 /\ 6
  , 2 /\ 7
  , 2 /\ 8
  , 2 /\ 9
  , 2 /\ 10
  , 3 /\ 4
  , 3 /\ 5
  , 3 /\ 6
  , 3 /\ 7
  , 3 /\ 8
  , 3 /\ 9
  , 3 /\ 10
  , 3 /\ 11
  , 4 /\ 3
  , 4 /\ 4
  , 4 /\ 11
  , 4 /\ 12
  , 5 /\ 3
  , 5 /\ 12
  , 6 /\ 3
  , 6 /\ 9
  , 6 /\ 10
  , 6 /\ 12
  , 7 /\ 3
  , 7 /\ 8
  , 7 /\ 10
  , 7 /\ 12
  , 8 /\ 1
  , 8 /\ 2
  , 8 /\ 3
  , 8 /\ 9
  , 8 /\ 12
  , 8 /\ 13
  , 8 /\ 14
  , 9 /\ 0
  , 9 /\ 3
  , 9 /\ 5
  , 9 /\ 7
  , 9 /\ 12
  , 9 /\ 15
  , 10 /\ 0
  , 10 /\ 3
  , 10 /\ 5
  , 10 /\ 6
  , 10 /\ 9
  , 10 /\ 12
  , 10 /\ 15
  , 11 /\ 0
  , 11 /\ 3
  , 11 /\ 6
  , 11 /\ 8
  , 11 /\ 10
  , 11 /\ 12
  , 11 /\ 15
  , 12 /\ 1
  , 12 /\ 2
  , 12 /\ 3
  , 12 /\ 8
  , 12 /\ 10
  , 12 /\ 12
  , 12 /\ 13
  , 12 /\ 14
  , 13 /\ 9
  ]

sky :: Preset
sky = presetV1'
  { key: D // natural
  , root: 0
  , scale: ScaleType.Hexatonic
  , wave: Wave.Sawtooth
  }
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

galaxy :: Preset
galaxy = presetV1'
  { key: A // natural
  , root: 3
  , scale: ScaleType.Pentatonic
  , wave: Wave.Sine
  }
  [ 1 /\ 7
  , 1 /\ 8
  , 1 /\ 9
  , 1 /\ 10
  , 2 /\ 3
  , 2 /\ 4
  , 2 /\ 6
  , 2 /\ 11
  , 3 /\ 2
  , 3 /\ 6
  , 3 /\ 11
  , 4 /\ 2
  , 4 /\ 7
  , 4 /\ 8
  , 5 /\ 2
  , 5 /\ 5
  , 5 /\ 10
  , 5 /\ 11
  , 6 /\ 2
  , 6 /\ 5
  , 6 /\ 9
  , 6 /\ 12
  , 7 /\ 3
  , 7 /\ 4
  , 7 /\ 9
  , 7 /\ 12
  , 8 /\ 6
  , 8 /\ 7
  , 8 /\ 12
  , 9 /\ 3
  , 9 /\ 8
  , 9 /\ 12
  , 10 /\ 3
  , 10 /\ 8
  , 10 /\ 10
  , 10 /\ 11
  , 11 /\ 4
  , 11 /\ 5
  , 11 /\ 6
  , 11 /\ 7
  ]

flower :: Preset
flower = presetV1
  [ 0 /\ 7
  , 1 /\ 6
  , 1 /\ 8
  , 2 /\ 6
  , 2 /\ 8
  , 3 /\ 7
  , 5 /\ 2
  , 5 /\ 3
  , 5 /\ 7
  , 5 /\ 11
  , 5 /\ 12
  , 6 /\ 1
  , 6 /\ 4
  , 6 /\ 6
  , 6 /\ 7
  , 6 /\ 8
  , 6 /\ 10
  , 6 /\ 13
  , 7 /\ 2
  , 7 /\ 3
  , 7 /\ 7
  , 7 /\ 11
  , 7 /\ 12
  , 9 /\ 7
  , 10 /\ 6
  , 10 /\ 8
  , 11 /\ 6
  , 11 /\ 8
  , 12 /\ 7
  ]

spaceship :: Preset
spaceship = presetV1'
  { key: G // natural
  , root: 0
  , scale: ScaleType.Hexatonic
  , wave: Wave.Triangle
  }
  [ 6 /\ 0
  , 6 /\ 3
  , 7 /\ 4
  , 8 /\ 0
  , 8 /\ 4
  , 9 /\ 1
  , 9 /\ 2
  , 9 /\ 3
  , 9 /\ 4
  ]

shipsPassing :: Preset
shipsPassing = presetV1'
  { key: G // natural
  , root: 0
  , scale: ScaleType.Hexatonic
  , wave: Wave.Triangle
  }
  [ 0 /\ 0
  , 0 /\ 3
  , 1 /\ 4
  , 2 /\ 0
  , 2 /\ 4
  , 3 /\ 1
  , 3 /\ 2
  , 3 /\ 3
  , 3 /\ 4
  , 12 /\ 11
  , 12 /\ 12
  , 12 /\ 13
  , 12 /\ 14
  , 13 /\ 11
  , 13 /\ 15
  , 14 /\ 11
  , 15 /\ 12
  , 15 /\ 15
  ]

pulsar :: Preset
pulsar = presetV1'
  { key: A // flat
  , root: 2
  , scale: ScaleType.Hexatonic
  , wave: Wave.Triangle
  }
  [ 1 /\ 3
  , 1 /\ 4
  , 1 /\ 10
  , 1 /\ 11
  , 2 /\ 4
  , 2 /\ 5
  , 2 /\ 9
  , 2 /\ 10
  , 3 /\ 1
  , 3 /\ 4
  , 3 /\ 6
  , 3 /\ 8
  , 3 /\ 10
  , 3 /\ 13
  , 4 /\ 1
  , 4 /\ 2
  , 4 /\ 3
  , 4 /\ 5
  , 4 /\ 6
  , 4 /\ 8
  , 4 /\ 9
  , 4 /\ 11
  , 4 /\ 12
  , 4 /\ 13
  , 5 /\ 2
  , 5 /\ 4
  , 5 /\ 6
  , 5 /\ 8
  , 5 /\ 10
  , 5 /\ 12
  , 6 /\ 3
  , 6 /\ 4
  , 6 /\ 5
  , 6 /\ 9
  , 6 /\ 10
  , 6 /\ 11
  , 8 /\ 3
  , 8 /\ 4
  , 8 /\ 5
  , 8 /\ 9
  , 8 /\ 10
  , 8 /\ 11
  , 9 /\ 2
  , 9 /\ 4
  , 9 /\ 6
  , 9 /\ 8
  , 9 /\ 10
  , 9 /\ 12
  , 10 /\ 1
  , 10 /\ 2
  , 10 /\ 3
  , 10 /\ 5
  , 10 /\ 6
  , 10 /\ 8
  , 10 /\ 9
  , 10 /\ 11
  , 10 /\ 12
  , 10 /\ 13
  , 11 /\ 1
  , 11 /\ 4
  , 11 /\ 6
  , 11 /\ 8
  , 11 /\ 10
  , 11 /\ 13
  , 12 /\ 4
  , 12 /\ 5
  , 12 /\ 9
  , 12 /\ 10
  , 13 /\ 3
  , 13 /\ 4
  , 13 /\ 10
  , 13 /\ 11
  ]

-- Configuration
-- TODO: Make all of these configurable

defaultBeatsPerMeasure :: Int
defaultBeatsPerMeasure = 16

defaultKey :: PitchClass
defaultKey = A // natural

defaultOctave :: Int
defaultOctave = 3

defaultNotes :: Int
defaultNotes = 16
