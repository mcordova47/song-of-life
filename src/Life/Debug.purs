module Life.Debug
  ( displayCells
  )
  where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Tuple.Nested (type (/\), (/\))


displayCells :: ∀ a b f. Show a => Show b => Foldable f => f (a /\ b) -> String
displayCells =
  Array.fromFoldable
  >>> Array.mapWithIndex (\i cell -> leftPad i <> displayCell cell <> "\n")
  >>> Array.fold
  >>> (_ <> "]")
  where
    displayCell (col /\ row) = show col <> " /\\ " <> show row
    leftPad i
      | i == 0 = "[ "
      | otherwise = ", "
