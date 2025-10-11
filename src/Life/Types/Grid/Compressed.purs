module Life.Types.Grid.Compressed
  ( cellsCodec
  , codec
  )
  where


import Data.Profunctor (dimap)
import Data.Set (Set)
import Data.Tuple.Nested ((/\))
import Life.Types.Cell (Cell)
import Life.Types.Codec (Codec, (</>))
import Life.Types.Grid (Grid)
import Life.Types.Grid as G
import Life.Types.Grid.Bounds as Bounds
import Life.Types.Grid.Instruction.Compressed as IC

codec :: Codec String Grid
codec = dimap toTuple fromTuple tupleCodec
  where
    toTuple { bounds, instructions } = bounds /\ instructions
    fromTuple (bounds /\ instructions) = { bounds, instructions }
    tupleCodec = Bounds.codec </> IC.arrayCodec

cellsCodec :: Codec String (Set Cell)
cellsCodec = dimap G.fromCells G.toCells codec
