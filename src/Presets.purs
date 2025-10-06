module Presets
  ( bottomRightGlider
  , collision
  , glider
  , heart
  )
  where

import Cell (Cell)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested ((/\))

heart :: Set Cell
heart = Set.fromFoldable
  [ 1 /\ 3
  , 1 /\ 4
  , 1 /\ 5
  , 1 /\ 9
  , 1 /\ 10
  , 1 /\ 11
  , 2 /\ 2
  , 2 /\ 6
  , 2 /\ 8
  , 2 /\ 12
  , 3 /\ 1
  , 3 /\ 7
  , 3 /\ 13
  , 4 /\ 0
  , 4 /\ 14
  , 5 /\ 0
  , 5 /\ 7
  , 5 /\ 14
  , 6 /\ 1
  , 6 /\ 6
  , 6 /\ 7
  , 6 /\ 8
  , 6 /\ 13
  , 7 /\ 2
  , 7 /\ 7
  , 7 /\ 12
  , 8 /\ 3
  , 8 /\ 11
  , 9 /\ 4
  , 9 /\ 10
  , 10 /\ 5
  , 10 /\ 9
  , 11 /\ 6
  , 11 /\ 8
  , 12 /\ 7
  ]

glider :: Set Cell
glider = Set.fromFoldable
  [ 0 /\ 2
  , 1 /\ 0
  , 1 /\ 2
  , 2 /\ 1
  , 2 /\ 2
  ]

bottomRightGlider :: Set Cell
bottomRightGlider = Set.fromFoldable
  [ 12 /\ 13
  , 11 /\ 15
  , 11 /\ 13
  , 10 /\ 14
  , 10 /\ 13
  ]

collision :: Set Cell
collision = Set.union glider bottomRightGlider
