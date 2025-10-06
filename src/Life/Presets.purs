module Life.Presets
  ( all
  , bottomRightGlider
  , collision
  , glider
  , heart
  , musicNotes
  , octocat
  )
  where

import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Life.Cell (Cell)

all :: Array (Set Cell)
all = [heart, octocat, musicNotes, glider, collision]

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

octocat :: Set Cell
octocat = Set.fromFoldable
  [ 1 /\ 5
  , 1 /\ 6
  , 1 /\ 10
  , 1 /\ 11
  , 2 /\ 5
  , 2 /\ 7
  , 2 /\ 8
  , 2 /\ 9
  , 2 /\ 11
  , 3 /\ 4
  , 3 /\ 12
  , 4 /\ 4
  , 4 /\ 12
  , 5 /\ 4
  , 5 /\ 12
  , 6 /\ 5
  , 6 /\ 11
  , 7 /\ 3
  , 7 /\ 6
  , 7 /\ 10
  , 8 /\ 4
  , 8 /\ 5
  , 8 /\ 7
  , 8 /\ 9
  , 9 /\ 6
  , 9 /\ 10
  , 10 /\ 6
  , 10 /\ 10
  ]

musicNotes :: Set Cell
musicNotes = Set.fromFoldable
  [ 1 /\ 9
  , 1 /\ 10
  , 2 /\ 7
  , 2 /\ 8
  , 2 /\ 10
  , 3 /\ 5
  , 3 /\ 6
  , 3 /\ 10
  , 4 /\ 3
  , 4 /\ 4
  , 4 /\ 6
  , 4 /\ 10
  , 4 /\ 11
  , 4 /\ 12
  , 5 /\ 6
  , 5 /\ 10
  , 5 /\ 13
  , 6 /\ 6
  , 6 /\ 7
  , 6 /\ 8
  , 6 /\ 11
  , 6 /\ 12
  , 7 /\ 6
  , 7 /\ 9
  , 8 /\ 7
  , 8 /\ 8
  ]
