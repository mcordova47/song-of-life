module Life.Types.Grid.Bounds where


import Data.Profunctor (dimap)
import Data.Tuple.Nested ((/\))
import Life.Types.Cell (Cell)
import Life.Types.Codec (Codec)
import Life.Types.Codec as Codec

type Bounds =
  { start :: Cell
  , cols :: Int
  }

codec :: Codec String Bounds
codec = dimap toTuple fromTuple tupleCodec
  where
    toTuple { cols, start } = cols /\ start
    fromTuple (cols /\ start) = { cols, start }

    tupleCodec = Codec.ljoin "c" Codec.int startCodec
    startCodec = Codec.ljoin "." Codec.int Codec.int
