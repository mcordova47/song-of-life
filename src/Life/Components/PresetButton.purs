module Life.Components.PresetButton
  ( Args
  , component
  )
  where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Elmish (ReactElement, (<|))
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Elmish.Hooks as Hooks
import Life.Types.Life (class VisibleAutomaton)
import Life.Types.Life as Life
import Life.Types.NamedRule as NamedRule

type Args f =
  { name :: String
  , life :: f Boolean
  , rows :: Int
  , cols :: Int
  , onClick :: E.EventHandler E.SyntheticEvent
  }

component :: forall f. VisibleAutomaton f => Args f -> ReactElement
component { name, life, rows, cols, onClick } = Hooks.component Hooks.do
  game /\ setGame <- Hooks.useState life
  hovering /\ setHovering <- Hooks.useState false
  let
    cells = Life.toCells game
    viewGame = if hovering then game else life

  Hooks.useEffect' { hovering, cells } \deps -> do
    if deps.hovering then do
      delay $ Milliseconds 200.0
      liftEffect $ setGame $ Life.step NamedRule.default game
    else
      liftEffect $ setGame life

  Hooks.pure $
    H.fragment
    [ H.div_ "preset d-flex rounded overflow-hidden border"
        { onClick
        , onMouseOver: setHovering <| true
        , onMouseLeave: setHovering <| false
        , onTouchStart: setHovering <| true
        , onTouchEnd: setHovering <| false
        } $
        H.div "preset-grid mx-auto" $
          Life.foldMap
            { life: viewGame
            , rows
            , cols
            , renderRow: H.div_ "d-flex" { style: H.css { lineHeight: 0 } }
            , renderCol: \living ->
                H.div "d-inline-block m-0 preset-grid-cell-container" $
                  H.div ("d-inline-block preset-grid-cell bg-" <> if living then "salmon" else "light")
                    H.empty
            }
    , H.h6 "mt-1 text-center" name
    ]
