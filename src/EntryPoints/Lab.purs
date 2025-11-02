module EntryPoints.Lab
  ( main
  )
  where

import Prelude

import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Elmish (ReactElement)
import Elmish.Boot (defaultMain)
import Elmish.HTML.Styled as H
import Life.Components.GridScene as GridScene
import Life.Components.Header as Header
import Life.Types.Game.Engines.Optimized.Unbounded (Unbounded)
import Life.Types.Game.Life as Life
import Life.Types.Grid.Cell (Cell)

main :: Effect Unit
main = defaultMain
  { def:
      { init: pure unit
      , view: const $ const view
      , update: const absurd
      }
  , elementId: "app"
  }

view :: ReactElement
view =
  H.div "vh-100 d-flex flex-column"
  [ Header.view
  , H.div "flex-grow-1 d-flex justify-content-center align-items-center" $
      GridScene.component
        { playing: true
        , game: Life.fromCells@Unbounded 0 0 start
        , height: 500
        , width: 500
        , defaultZoom: 3.0
        , backgroundColor: "#0D0208"
        , cellColor: "#00FF41"
        }
  , H.style "" """
      body { padding-bottom: 0 !important; }
    """
  ]

-- "rabbits"
start :: Set Cell
start = Set.fromFoldable
  [ -1 /\ -1
  , -1 /\ 4
  , 0 /\ -3
  , 0 /\ -2
  , 1 /\ -2
  , 1 /\ -1
  , 1 /\ 1
  , 1 /\ 2
  , 1 /\ 3
  ]
