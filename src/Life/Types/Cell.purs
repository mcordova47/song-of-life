module Life.Types.Cell
  ( Cell
  , random
  )
  where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Random as R

type Cell = Int /\ Int

random :: { x1 :: Int, x2 :: Int, y1 :: Int, y2 :: Int } -> Effect Cell
random { x1, x2, y1, y2 } =
  (/\) <$> R.randomInt y1 y2 <*> R.randomInt x1 x2
