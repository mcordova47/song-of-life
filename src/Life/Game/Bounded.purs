module Life.Game.Bounded
  ( Bounded
  , fromCells
  , random
  , step
  , toCells
  )
  where

import Prelude

import Control.Alternative (guard)
import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Data.Array ((!!), (..))
import Data.Array as Array
import Data.Foldable (fold, foldl)
import Data.Maybe (fromMaybe, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Random as R
import Life.Types.Cell (Cell)
import Life.Types.Life (class InteractiveLife, class Life, class VisibleLife)
import Life.Types.Life as Life
import Life.Utils as U

newtype Bounded a = Bounded
  { grid :: Array (Array a)
  , focus :: Int /\ Int /\ a
  }
derive newtype instance Eq a => Eq (Bounded a)

instance Functor Bounded where
  map f (Bounded b@{ focus: row /\ col /\ x }) =
    Bounded { grid: map f <$> b.grid, focus: row /\ col /\ f x }

instance Extend Bounded where
  extend f (Bounded b@{ focus: r /\ c /\ _ }) = Bounded { grid: grid', focus }
    where
      grid' =
        b.grid # Array.mapWithIndex \row cols ->
          cols # Array.mapWithIndex \col x ->
            f $ Bounded { grid: b.grid, focus: row /\ col /\ x }

      focus = r /\ c /\ f (Bounded b)

instance Comonad Bounded where
  extract (Bounded { focus: _ /\ _ /\ x }) = x

instance Life Bounded where
  neighbors p (Bounded b@{ focus: row /\ col /\ _ }) =
    Array.length $ Array.filter identity do
      row' <- [row - 1, row, row + 1]
      col' <- [col - 1, col, col + 1]
      guard (row /= row' || col /= col')
      pure $ (b.grid !! row' # fromMaybe []) !! col' # maybe false p

instance VisibleLife Bounded where
  grid rows cols (Bounded b) =
    Array.take cols <$> Array.take rows b.grid

instance InteractiveLife Bounded where
  update f row col (Bounded b) = Bounded b
    { grid = U.tryModifyAt row (U.tryModifyAt col f) b.grid }

fromCells :: Int -> Int -> Set Cell -> Bounded Boolean
fromCells rows cols livingCells =
  Bounded { grid: grid', focus: 0 /\ 0 /\ focused 0 0 }
  where
    grid' =
      (0 .. (rows - 1)) <#> \row ->
        (0 .. (cols - 1)) <#> \col ->
          focused row col

    focused row col =
      Set.member (row /\ col) livingCells

toCells :: Bounded Boolean -> Set Cell
toCells (Bounded b) = foldGrid indexedGrid
  where
    foldGrid = Set.empty # foldl \acc ->
      acc # foldl \acc' (row /\ col /\ living) ->
        if living then Set.insert (row /\ col) acc' else acc'

    indexedGrid =
      b.grid # Array.mapWithIndex \row cols ->
        cols # Array.mapWithIndex \col living ->
          (row /\ col /\ living)

step :: forall r. { livingCells :: Set Cell | r } -> Int -> Int -> Set Cell
step { livingCells } rows cols =
  livingCells
  # fromCells rows cols
  # Life.step
  # toCells

-- TODO: figure out how to bias towards small clusters
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
