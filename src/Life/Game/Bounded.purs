module Life.Game.Bounded
  ( grid
  , random
  , step
  , transpose
  )
  where

import Prelude

import Control.Alternative (guard)
import Data.Array ((..))
import Data.Array as A
import Data.Foldable (fold, foldl)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Random as R
import Life.Types.Cell (Cell)

step :: forall r. { livingCells :: Set Cell | r } -> Int -> Int -> Set Cell
step s rows cols = foldl stepRow s.livingCells $ grid rows cols
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

grid :: Int -> Int -> Array (Array Cell)
grid numRows numCols =
  rows <#> \row -> cols <#> \col -> row /\ col
  where
    rows = 0 .. (numRows - 1)
    cols = 0 .. (numCols - 1)

transpose :: forall a. Array (Array a) -> Array (Array a)
transpose rows = case A.head rows of
  Just row -> transpose' (A.length row) rows
  Nothing -> []
  where
    transpose' n = A.replicate n [] # foldl \cols row ->
      A.zipWith (<>) cols (row <#> A.singleton)

-- TODO: instead of a bounding box, limit rows per column
random :: Int -> Int -> Effect (Set Cell)
random rows cols = Set.fromFoldable <<< fold <$> for (0 .. (cols - 1)) \col -> do
  numRows <- R.randomInt 0 maxRows
  extra <- R.randomInt 0 (rows - colGroup)
  for (1 .. numRows) \_ -> do
    row <- R.randomInt 0 (colGroup - 1)
    pure ((row + extra) /\ col)
  where
    colGroup = 6
    maxRows = 5
