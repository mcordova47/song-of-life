module Life.Types.Grid
  ( Grid
  , cellsCodec
  , codec
  , fromCells
  , toCells
  )
  where

import Prelude

import Data.Array ((..))
import Data.Array as Array
import Data.List (List(..), (:))
import Data.Maybe (fromMaybe)
import Data.Profunctor (dimap)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (maximum, minimum)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Life.Types.Cell (Cell)
import Life.Types.Codec (Codec, (</>))
import Life.Types.Grid.Bounds (Bounds)
import Life.Types.Grid.Bounds as Bounds
import Life.Types.Grid.Instruction (Instruction(..))
import Life.Types.Grid.Instruction as Instructions

type Grid = 
  { bounds :: Bounds
  , instructions :: Array Instruction
  }

codec :: Codec String Grid
codec = dimap toTuple fromTuple tupleCodec
  where
    toTuple { bounds, instructions } = bounds /\ instructions
    fromTuple (bounds /\ instructions) = { bounds, instructions }
    tupleCodec = Bounds.codec </> Instructions.arrayCodec

cellsCodec :: Codec String (Set Cell)
cellsCodec = dimap fromCells toCells codec

fromCells :: Set Cell -> Grid
fromCells cells = { bounds, instructions }
  where
    bounds =
      { start: minRow /\ minCol
      , cols
      }

    instructions = indices
      # Array.foldl go { instructions: Nil, position: 0 }
      # _.instructions
      # Array.fromFoldable
      # Array.reverse
      where
        go acc i =
          { position: i
          , instructions: case acc.instructions of
              Nil | i == 0 -> TurnOn 1 : Nil
              Nil -> TurnOn 1 : Move i : Nil
              Move _ : _ | i == acc.position + 1 -> TurnOn 1 : acc.instructions
              Move n : rest -> TurnOn 1 : Move (n + i - acc.position) : rest
              TurnOn n : rest | i == acc.position + 1 -> TurnOn (n + 1) : rest
              TurnOn _ : _ -> TurnOn 1 : Move (i - acc.position) : acc.instructions
          }

    cellArray = Array.fromFoldable cells

    minCol = cellArray <#> snd # minimum # fromMaybe 0
    maxCol = cellArray <#> snd # maximum # fromMaybe 0
    minRow = cellArray <#> fst # minimum # fromMaybe 0
    cols = maxCol - minCol + 1

    indices = cellArray <#> \(row /\ col) -> (row - minRow) * cols + col - minCol

toCells :: Grid -> Set Cell
toCells { bounds, instructions } = indices <#> indexToCell # Set.fromFoldable
  where
    indexToCell i
      | i <= 0 = bounds.start
      | otherwise = (fst bounds.start + i / bounds.cols) /\ (snd bounds.start + mod i bounds.cols)

    indices = instructions
      # Array.foldl go { indices: [], position: 0 }
      # _.indices
      where
        go acc = case _ of
          Move n ->
            acc { position = acc.position + n }
          TurnOn n ->
            acc
              { indices = acc.indices <> (acc.position .. (n + acc.position - 1))
              , position = acc.position + n - 1
              }
