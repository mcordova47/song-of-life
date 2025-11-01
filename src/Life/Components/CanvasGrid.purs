module Life.Components.CanvasGrid
  ( Args
  , ComponentArgs
  , Controls
  , component
  , init
  , update
  , view
  )
  where

import Prelude

import Control.Alternative (guard)
import Data.Array (fold, (..))
import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Set as Set
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Elmish (ReactElement, Dispatch)
import Elmish.HTML.Events (MouseEvent(..))
import Elmish.HTML.Styled as H
import Elmish.Hooks as Hooks
import Life.Components.Canvas (CanvasElement)
import Life.Components.Canvas as Canvas
import Life.HTML.Events.WheelEvent (WheelEvent(..))
import Life.HTML.Events.WheelEvent as WE
import Life.Types.Life (class CellularAutomaton, class InteractiveAutomaton)
import Life.Types.Life as Life
import Life.Types.NamedRule (NamedRule)
import Life.Utils as U
import Web.DOM.Element (getBoundingClientRect)
import Web.Event.Event (preventDefault, stopPropagation)

type Args r =
  { playing :: Boolean
  , speed :: Int
  , rule :: NamedRule
  , controls :: Controls -> ReactElement
  | r
  }

type ComponentArgs f r = Args
  ( width :: Int
  , height :: Int
  , game :: f Boolean
  | r
  )

type Controls =
  { next :: Effect Unit
  , reset :: Effect Unit
  , currentStep :: Int
  }

type Props =
  { playing :: Boolean
  , rule :: NamedRule
  , step :: Int
  , speed :: Int
  }

type State f =
  { buffer :: Number
  , dragging :: Maybe { startX :: Number, startY :: Number, offsetX :: Number, offsetY :: Number }
  , game :: f Boolean
  , origin :: Number /\ Number
  , zoom :: Number
  }

component :: forall f r. Eq (f Boolean) => InteractiveAutomaton f => ComponentArgs f r -> ReactElement
component args = Hooks.component Hooks.do
  dragged /\ setDragged <- Hooks.useState false
  step /\ setStep <- Hooks.useState 0

  Hooks.pure $
    Canvas.component (if dragged then "cursor-grabbing" else "cursor-pointer")
      { id: "canvas"
      , width: args.width
      , height: args.height
      , fill: "#f5f5f5"
      , init: init args
      , props:
          { playing: args.playing
          , rule: args.rule
          , step
          , speed: args.speed
          }
      , update: update
          { playing: args.playing
          , speed: args.speed
          , rule: args.rule
          , controls: args.controls
          , width: args.width
          , height: args.height
          , game: args.game
          , setStep
          , dragged
          , setDragged
          }
      , view: view args
      , render: \setState canvas ->
          H.fragment
          [ args.controls
              { next: do
                  setState \s -> s { game = Life.steps 1 args.rule s.game }
                  setStep (step + 1)
              , reset: do
                  setState \s -> s { game = Life.empty 0 0 }
                  setStep 0
              , currentStep: step
              }
          , canvas
          ]
      }

init :: forall f r. ComponentArgs f r -> State f
init args =
  { buffer: 0.0
  , dragging: Nothing
  , game: args.game
  , origin: 0.0 /\ 0.0
  , zoom: 5.0
  }

update :: forall f
  . InteractiveAutomaton f
  => ComponentArgs f ( setStep :: Dispatch Int, setDragged :: Dispatch Boolean, dragged :: Boolean )
  -> Props
  -> State f
  -> Canvas.Message
  -> Effect (State f)
update args props state = case _ of
  Canvas.Tick (Milliseconds ms) | props.playing -> do
    let
      duration = ms / 1000.0
      stepsPerSecond = Int.floor $ Number.pow 10.0 (Int.toNumber props.speed / 50.0)
      newSteps = if props.playing then Int.toNumber stepsPerSecond * duration else 0.0
      accumulatedSteps = state.buffer + newSteps
      presentSteps = Int.floor accumulatedSteps
      buffer = max 0.0 (accumulatedSteps - Int.toNumber presentSteps)
      game = if presentSteps > 0 then Life.steps presentSteps props.rule state.game else state.game

    when (presentSteps > 0) $
      args.setStep $ props.step + presentSteps

    pure state { buffer = buffer, game = game }
  Canvas.Tick _ ->
    pure state { buffer = 0.0 }
  Canvas.MouseDown (MouseEvent e) -> do
    let offsetX /\ offsetY = offset args state
    pure state
      { dragging = Just
          { startX: e.clientX - offsetX
          , startY: e.clientY - offsetY
          , offsetX
          , offsetY
          }
      }
  Canvas.MouseMove (MouseEvent e) ->
    case state.dragging of
      Just d -> do
        let
          offsetX' = e.clientX - d.startX
          offsetY' = e.clientY - d.startY
          hypotenuse = Number.sqrt (Number.pow (offsetX' - d.offsetX) 2.0 + Number.pow (offsetY' - d.offsetY) 2.0)

        when (hypotenuse >= 2.0) $
          args.setDragged true

        if args.dragged then
          pure state
            { origin =
                (numCols args state / 2.0 - offsetX' / state.zoom - 0.5) /\
                (numRows args state / 2.0 - offsetY' / state.zoom - 0.5)
            }
        else
          pure state
      Nothing ->
        pure state
  Canvas.MouseUp (MouseEvent e) -> do
    if args.dragged then do
      args.setDragged false
      pure state { dragging = Nothing }
    else do
      rect <- getBoundingClientRect e.target
      let
        offsetX /\ offsetY = offset args state
        x = e.clientX - rect.left - offsetX
        y = e.clientY - rect.top - offsetY
        col = Int.floor (x / state.zoom)
        row = Int.floor (y / state.zoom)
      pure state { dragging = Nothing, game = Life.toggle row col state.game }
  Canvas.Wheel we@(WheelEvent e) -> do
    preventDefault e
    stopPropagation e
    let
      zoomFactor = clamp 0.85 1.15 (1.0 - 0.015 * WE.deltaY we)
      zoom = max 1.0 $ min 50.0 (state.zoom * zoomFactor)
    pure state { zoom = zoom }

view :: forall f r. CellularAutomaton f => ComponentArgs f r -> State f -> CanvasElement
view args state = fold
  [ Canvas.Fragment $
      state.game # Life.toCells # Set.filter isVisible # Array.fromFoldable <#> \(row /\ col) ->
        Canvas.Rect
          { position:
              { x: Int.toNumber col * state.zoom + offsetX
              , y: Int.toNumber row * state.zoom + offsetY
              }
          , height: state.zoom
          , width: state.zoom
          , fill: "#ff75aa"
          }
  , gridLineConfig # foldMap \config ->
      Canvas.Fragment
      [ Canvas.Fragment $
          (0 .. Int.ceil (numCols args state)) <#> \col ->
            Canvas.Line
              { start: { x: colX config col, y: 0.0 }
              , end: { x: colX config col, y: Int.toNumber args.height }
              , stroke: config.stroke
              }
      , Canvas.Fragment $
          (0 .. Int.ceil (numRows args state)) <#> \row ->
            Canvas.Line
              { start: { x: 0.0, y: rowY config row }
              , end: { x: Int.toNumber args.width, y: rowY config row }
              , stroke: config.stroke
              }
      ]
  ]
  where
    offsetX /\ offsetY = offset args state

    bounds =
      { maxX: Int.ceil (numCols args state - offsetX / state.zoom)
      , maxY: Int.ceil (numRows args state - offsetY / state.zoom)
      , minX: Int.floor (-offsetX / state.zoom)
      , minY: Int.floor (-offsetY / state.zoom)
      }

    isVisible (r /\ c) =
      r <= maxY && r >= minY && c <= maxX && c >= minX
      where
        { maxX, maxY, minX, minY } = bounds

    colX config col = Int.toNumber col * state.zoom + config.adjustX
    rowY config row = Int.toNumber row * state.zoom + config.adjustY

    gridLineConfig :: Maybe _
    gridLineConfig = do
      guard (state.zoom >= 10.0)
      let
        opacity = min 255 ((Int.floor state.zoom - 10) * 2)
        hex = Int.toStringAs Int.hexadecimal opacity # U.padLeft 2 "0"
        stroke = "#575757" <> hex
        adjustX = Number.remainder offsetX state.zoom
        adjustY = Number.remainder offsetY state.zoom
      pure
        { stroke
        , adjustX
        , adjustY
        }

offset :: forall f r. ComponentArgs f r -> State f -> Number /\ Number
offset args state@{ origin: originX /\ originY } =
  (state.zoom * (numCols args state / 2.0 - originX - 0.5)) /\
  state.zoom * (numRows args state / 2.0 - originY - 0.5)

numRows :: forall f r. ComponentArgs f r -> State f -> Number
numRows args state = Int.toNumber args.height / state.zoom

numCols :: forall f r. ComponentArgs f r -> State f -> Number
numCols args state = Int.toNumber args.width / state.zoom
