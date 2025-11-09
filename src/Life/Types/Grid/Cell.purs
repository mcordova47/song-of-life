module Life.Types.Grid.Cell
  ( Cell
  , add
  , adjust
  , fromAscii
  , neighbors
  , random
  , randomSet
  , range
  )
  where

import Prelude hiding (add)

import Control.Alternative (guard)
import Data.Array ((..))
import Data.Array as Array
import Data.Foldable (fold)
import Data.Set (Set)
import Data.Set as Set
import Data.String as S
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Random as R
import Life.Utils.Array as A

type Cell = Int /\ Int

random :: { x1 :: Int, x2 :: Int, y1 :: Int, y2 :: Int } -> Effect Cell
random { x1, x2, y1, y2 } =
  (/\) <$> R.randomInt y1 y2 <*> R.randomInt x1 x2

-- TODO: figure out how to bias towards small clusters
randomSet :: Int -> Int -> Effect (Set Cell)
randomSet rows cols = Set.fromFoldable <<< fold <$> for (0 .. (cols - 1)) \col -> do
  numRows <- R.randomInt 0 maxRows
  extra <- R.randomInt 0 (rows - colGroup)
  for (1 .. numRows) \_ -> do
    row <- R.randomInt 0 (colGroup - 1)
    pure ((row + extra) /\ col)
  where
    colGroup = 6
    maxRows = 5

range :: Int -> Int -> Set Cell
range rows cols = Set.fromFoldable do
  row <- 0 .. (rows - 1)
  col <- 0 .. (cols - 1)
  pure (row /\ col)

neighbors :: Cell -> Array Cell
neighbors (row /\ col) = do
  row' <- [row - 1, row, row + 1]
  col' <- [col - 1, col, col + 1]
  guard (row' /= row || col' /= col)
  pure (row' /\ col')

-- | Parses a set of cells from an ASCII representation of a grid, where "#"
-- | represents living cells and any non-whitespace character is a dead cell.
-- | E.g., a glider would be:
-- |
-- | fromAscii """
-- |   . . #
-- |   # . #
-- |   . # #
-- | """
fromAscii :: String -> Array Cell
fromAscii =
  S.split (S.Pattern "\n") >>>
  map S.trim >>>
  Array.filter (not S.null) >>>
  Array.mapWithIndex (\row str -> { row, str }) >=>
  \{ row, str } -> str
    # S.split (S.Pattern "")
    # Array.filter (not S.null <<< S.trim)
    # A.findIndices ((==) "#")
    <#> ((/\) row)

add :: Cell -> Cell -> Cell
add (r1 /\ c1) (r2 /\ c2) = (r1 + r2) /\ (c1 + c2)

adjust :: Cell -> Array Cell -> Array Cell
adjust cell = map (add cell)
