module Life.Types.Preset
  ( Config
  , Preset(..)
  , PresetV0
  , PresetV0'
  , PresetV1
  , State
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
import Life.Game.Patterns as P
import Life.Types.Codec (Codec, (/>), (</>), (<\>))
import Life.Types.Codec as Codec
import Life.Types.Game.Life (class CellularAutomaton)
import Life.Types.Game.Life as Life
import Life.Types.Grid as Grid
import Life.Types.Grid.Cell (Cell)
import Life.Types.Grid.Cell as Cell
import Life.Types.Grid.Compressed as GridCompressed
import Life.Types.Music.Letter (Letter(..))
import Life.Types.Music.Modifier (flat, natural)
import Life.Types.Music.PitchClass (PitchClass, (//))
import Life.Types.Music.PitchClass as PitchClass
import Life.Types.Music.ScaleType (ScaleType)
import Life.Types.Music.ScaleType as ScaleType
import Life.Types.Music.Wave (Wave)
import Life.Types.Music.Wave as Wave

-- | Represents a configuration for the game grid. It's versioned to avoid link
-- | rot.
data Preset
  = V0 PresetV0
  | V1 PresetV1

type PresetV0' r = Config ( livingCells :: Set Cell | r )

type PresetV0 = PresetV0' ()

type PresetV1 = PresetV0

type State f r = Config ( game :: f Boolean | r )

type Config r =
  { beatsPerMeasure :: Int
  , key :: PitchClass
  , notes :: Int
  , root :: Int
  , scale :: ScaleType
  , wave :: Wave
  | r
  }

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
fromCells = fromConfig <<<
  { beatsPerMeasure: defaultBeatsPerMeasure
  , key: defaultKey
  , notes: defaultNotes
  , root: 0
  , scale: ScaleType.default
  , livingCells: _
  , wave: Wave.default
  }

fromState :: forall f r. CellularAutomaton f => State f r -> Preset
fromState s = fromConfig
  { beatsPerMeasure: s.beatsPerMeasure
  , key: s.key
  , livingCells: Life.toCells s.game
  , notes: s.notes
  , root: s.root
  , scale: s.scale
  , wave: s.wave
  }

fromConfig :: forall r. PresetV0' r -> Preset
fromConfig s = V1
  { beatsPerMeasure: s.beatsPerMeasure
  , key: s.key
  , livingCells: s.livingCells
  , notes: s.notes
  , root: s.root
  , scale: s.scale
  , wave: s.wave
  }

toLife :: forall @f. CellularAutomaton f => Preset -> f Boolean
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
presetV1' p cells = fromConfig
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
heart = presetV1 $ Cell.adjust (0 /\ 1) P.heart

glider :: Preset
glider = presetV1 P.glider

collision :: Preset
collision = presetV1 $ Cell.adjust (2 /\ 1) P.collision

octocat :: Preset
octocat = presetV1 $ Cell.adjust (1 /\ 3) P.octocat

headphones :: Preset
headphones = presetV1'
  { key: A // flat
  , root: 0
  , scale: ScaleType.Hexatonic
  , wave: Wave.Square
  } $
  Cell.adjust (2 /\ 0) P.headphones

sky :: Preset
sky = presetV1'
  { key: D // natural
  , root: 0
  , scale: ScaleType.Hexatonic
  , wave: Wave.Sawtooth
  }
  P.sky

galaxy :: Preset
galaxy = presetV1'
  { key: A // natural
  , root: 3
  , scale: ScaleType.Pentatonic
  , wave: Wave.Sine
  } $
  Cell.adjust (1 /\ 2) P.galaxy

flower :: Preset
flower = presetV1 $ Cell.adjust (0 /\ 1) P.flower

spaceship :: Preset
spaceship = presetV1'
  { key: G // natural
  , root: 0
  , scale: ScaleType.Hexatonic
  , wave: Wave.Triangle
  } $
  Cell.adjust (6 /\ 0) P.spaceship

shipsPassing :: Preset
shipsPassing = presetV1'
  { key: G // natural
  , root: 0
  , scale: ScaleType.Hexatonic
  , wave: Wave.Triangle
  }
  P.shipsPassing

pulsar :: Preset
pulsar = presetV1'
  { key: A // flat
  , root: 2
  , scale: ScaleType.Hexatonic
  , wave: Wave.Triangle
  } $
  Cell.adjust (1 /\ 1) P.pulsar

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
