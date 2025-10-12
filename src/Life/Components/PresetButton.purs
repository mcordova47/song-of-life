module Life.Components.PresetButton
  ( Args
  , component
  )
  where

import Prelude

import Data.Array as Array
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Elmish (ReactElement, (<|))
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Elmish.Hooks as Hooks
import Life.Game as Game
import Life.Types.Cell (Cell)

type Args =
  { name :: String
  , cells :: Set Cell
  , grid :: Array (Array Cell)
  , onClick :: E.EventHandler E.SyntheticEvent
  }

component :: Args -> ReactElement
component { name, cells, grid, onClick } = Hooks.component Hooks.do
  livingCells /\ setLivingCells <- Hooks.useState cells
  hovering /\ setHovering <- Hooks.useState false
  let viewCells = if hovering then livingCells else cells

  Hooks.useEffect' { hovering, livingCells } \deps -> do
    if deps.hovering then do
      delay $ Milliseconds 200.0
      liftEffect $ setLivingCells $ Game.step { livingCells } $ Array.length grid
    else
      liftEffect $ setLivingCells cells

  Hooks.pure $
    H.fragment
    [ H.div_ "preset d-flex rounded overflow-hidden border"
        { onClick
        , onMouseOver: setHovering <| true
        , onMouseLeave: setHovering <| false
        } $
        H.div "preset-grid mx-auto" $
          grid <#> \row ->
            H.div_ "d-flex"
              { style: H.css { lineHeight: 0 } } $
              row <#> \cell ->
                H.div "d-inline-block m-0 preset-grid-cell-container" $
                  H.div ("d-inline-block preset-grid-cell bg-" <> if Set.member cell viewCells then "salmon" else "light")
                    H.empty
    , H.h6 "mt-1 text-center" name
    ]
