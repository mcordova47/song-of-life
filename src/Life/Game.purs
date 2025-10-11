module Life.Game where

import Prelude

import Control.Alternative (guard)
import Data.Array ((..))
import Data.Array as A
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Life.Music (Note, (<<), (>>))
import Life.Music as M
import Life.Types.Cell (Cell)

step :: Set Cell -> Set Cell
step livingCells = foldl stepRow livingCells grid
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
      if Set.member cell livingCells then acc + 1 else acc

    neighbors (row /\ col) = do
      row' <- [row - 1, row, row + 1]
      col' <- [col - 1, col, col + 1]
      guard $ (row' /\ col') /= (row /\ col)
      pure (row' /\ col')

grid :: Array (Array Cell)
grid =
  rows <#> \row -> cols <#> \col -> row /\ col
  where
    rows = 0 .. (A.length notes - 1)
    cols = 0 .. (beatsPerMeasure - 1)

transpose :: forall a. Array (Array a) -> Array (Array a)
transpose rows = case A.head rows of
  Just row -> transpose' (A.length row) rows
  Nothing -> []
  where
    transpose' n = A.replicate n [] # foldl \cols row ->
      A.zipWith (<>) cols (row <#> A.singleton)

-- Configuration
-- TODO: Make all of these configurable

beatsPerMeasure :: Int
beatsPerMeasure = 16

notes :: Array Note
notes = ((>>) root) <$>
  [ mempty
  , M.major3rd
  , M.perfect5th
  , M.major2nd
  , M.perfect4th
  , M.major6th
  , M.octave
  , M.octave <> M.major3rd
  , M.octave <> M.perfect5th
  , M.octave <> M.major2nd
  , M.octave <> M.perfect4th
  , M.octave <> M.major6th
  ]

root :: Note
root = M.a4 << M.octave
