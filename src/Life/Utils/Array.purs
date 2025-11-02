module Life.Utils.Array
  ( chunksOf
  , fill
  , grid
  , tryModifyAt
  )
  where

import Prelude

import Data.Array ((..))
import Data.Array as Array
import Data.Maybe (fromMaybe)
import Data.Tuple.Nested ((/\))
import Life.Types.Cell (Cell)

chunksOf :: forall a. Int -> Array a -> Array (Array a)
chunksOf n xs = Array.splitAt n xs # go
  where
    go { before, after }
      | Array.null before = []
      | Array.null after = [before]
      | otherwise = [before] <> chunksOf n after

fill :: forall a. Int -> a -> Array a -> Array a
fill n x xs =
  xs <> Array.replicate (n - Array.length xs) x

tryModifyAt :: forall a. Int -> (a -> a) -> Array a -> Array a
tryModifyAt i g xs = Array.modifyAt i g xs # fromMaybe xs

grid :: Int -> Int -> Array (Array Cell)
grid numRows numCols =
  rows <#> \row -> cols <#> \col -> row /\ col
  where
    rows = 0 .. (numRows - 1)
    cols = 0 .. (numCols - 1)
