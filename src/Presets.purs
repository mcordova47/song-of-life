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
  [ 0 /\ 3
  , 0 /\ 4
  , 0 /\ 5
  , 0 /\ 9
  , 0 /\ 10
  , 0 /\ 11
  , 1 /\ 2
  , 1 /\ 6
  , 1 /\ 8
  , 1 /\ 12
  , 2 /\ 1
  , 2 /\ 7
  , 2 /\ 13
  , 3 /\ 0
  , 3 /\ 14
  , 4 /\ 0
  , 4 /\ 7
  , 4 /\ 14
  , 5 /\ 1
  , 5 /\ 6
  , 5 /\ 7
  , 5 /\ 8
  , 5 /\ 13
  , 6 /\ 2
  , 6 /\ 7
  , 6 /\ 12
  , 7 /\ 3
  , 7 /\ 11
  , 8 /\ 4
  , 8 /\ 10
  , 9 /\ 5
  , 9 /\ 9
  , 10 /\ 6
  , 10 /\ 8
  , 11 /\ 7
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
  [ 11 /\ 13
  , 10 /\ 15
  , 10 /\ 13
  , 9 /\ 14
  , 9 /\ 13
  ]

collision :: Set Cell
collision = Set.union glider bottomRightGlider
