module Life.Types.Cell
  ( Cell
  )
  where

import Data.Tuple.Nested (type (/\))

type Cell = Int /\ Int
