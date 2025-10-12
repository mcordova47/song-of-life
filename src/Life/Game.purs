module Life.Game
  ( defaultKey
  , defaultOctave
  , defaultScale
  , grid
  , random
  , step
  , transpose
  )
  where

import Prelude

import Control.Alternative (guard)
import Data.Array ((..))
import Data.Array as A
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Random as R
import Life.Types.Cell (Cell)
import Life.Types.Cell as Cell
import Life.Types.Music.Letter (Letter(..))
import Life.Types.Music.Modifier (natural)
import Life.Types.Music.PitchClass (PitchClass, (//))
import Life.Types.Music.Scale (Scale)
import Life.Types.Music.Scale as Scale

step :: forall r. { livingCells :: Set Cell, key :: PitchClass | r } -> Set Cell
step s = foldl stepRow s.livingCells $ grid s
  where
    stepRow livingCells' row =
      foldl stepCell livingCells' row

    stepCell livingCells' cell =
      case livingNeighbors cell of
        3 -> Set.insert cell livingCells'
        2 -> livingCells'
        _ -> Set.delete cell livingCells'

    livingNeighbors cell =
      foldl countLiving 0 $ neighbors cell

    countLiving acc cell =
      if Set.member cell s.livingCells then acc + 1 else acc

    neighbors (row /\ col) = do
      row' <- [row - 1, row, row + 1]
      col' <- [col - 1, col, col + 1]
      guard $ (row' /\ col') /= (row /\ col)
      pure (row' /\ col')

grid :: forall r. { key :: PitchClass | r } -> Array (Array Cell)
grid { key } =
  rows <#> \row -> cols <#> \col -> row /\ col
  where
    rows = 0 .. (A.length (defaultScale key defaultOctave) - 1)
    cols = 0 .. (beatsPerMeasure - 1)

transpose :: forall a. Array (Array a) -> Array (Array a)
transpose rows = case A.head rows of
  Just row -> transpose' (A.length row) rows
  Nothing -> []
  where
    transpose' n = A.replicate n [] # foldl \cols row ->
      A.zipWith (<>) cols (row <#> A.singleton)

-- TODO: instead of a bounding box, limit rows per column
random :: forall r. { key :: PitchClass | r } -> Effect (Set Cell)
random { key } = do
  let
    x1 = 0
    x2 = beatsPerMeasure - 1
    notes = defaultScale key defaultOctave
  y1 <- R.randomInt 0 (A.length notes - 3)
  y2 <- R.randomInt (y1 + 2) (A.length notes - 1)
  numCells <- R.randomInt 5 $ Int.floor (Int.toNumber ((x2 - x1) * (y2 - y1)) * 0.75)
  cells <- for (A.replicate numCells unit) \_ -> Cell.random { x1, x2, y1, y2 }
  pure $ Set.fromFoldable cells

-- Configuration
-- TODO: Make all of these configurable

beatsPerMeasure :: Int
beatsPerMeasure = 16

defaultKey :: PitchClass
defaultKey = A // natural

defaultOctave :: Int
defaultOctave = 3

defaultScale :: Scale
defaultScale = Scale.hexatonic
