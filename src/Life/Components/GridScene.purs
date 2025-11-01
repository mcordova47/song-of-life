module Life.Components.GridScene
  ( Args
  , ComponentArgs
  , GridArgs
  , HookArgs
  , Props
  , State(..)
  , component
  , init
  , update
  , useGridScene
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
import Elmish (Dispatch, ReactElement)
import Elmish.HTML.Events (MouseEvent(..))
import Elmish.Hooks (type (<>), Hook, UseState, (=/>))
import Elmish.Hooks as Hooks
import Life.Components.Scene (UseScene, useScene)
import Life.Components.Scene as Scene
import Life.HTML.Events.WheelEvent (WheelEvent(..))
import Life.HTML.Events.WheelEvent as WE
import Life.Types.Life (class CellularAutomaton, class InteractiveAutomaton)
import Life.Types.Life as Life
import Life.Types.NamedRule (NamedRule)
import Life.Utils ((><))
import Life.Utils as U
import Web.DOM.Element (getBoundingClientRect)
import Web.Event.Event (preventDefault, stopPropagation)

type Args r =
  { playing :: Boolean
  , speed :: Int
  , rule :: NamedRule
  , step :: Int
  , onStep :: Dispatch Int
  | r
  }

type GridArgs r = Args
  ( width :: Int
  , height :: Int
  | r
  )

type HookArgs f r = GridArgs
  ( game :: f Boolean
  | r
  )

type ComponentArgs f r = HookArgs f
  ( render :: Scene.SetState (State f) -> ReactElement -> ReactElement
  | r
  )

type Props =
  { playing :: Boolean
  , rule :: NamedRule
  , step :: Int
  , speed :: Int
  }

newtype State f = State
  { buffer :: Number
  , dragging :: Maybe { startX :: Number, startY :: Number, offsetX :: Number, offsetY :: Number }
  , game :: f Boolean
  , origin :: Number /\ Number
  , zoom :: Number
  }
instance Eq (f Boolean) => Eq (State f) where
  eq (State a) (State b) =
    a.zoom == b.zoom &&
    a.origin == b.origin &&
    a.game == b.game

type UseGridScene f t = UseState Boolean <> UseScene Props (State f) <> t

component :: forall f r. Eq (f Boolean) => InteractiveAutomaton f => ComponentArgs f r -> ReactElement
component args = useGridScene args =/> flip args.render

useGridScene :: forall f r. Eq (f Boolean) => InteractiveAutomaton f => HookArgs f r -> Hook (UseGridScene f) (ReactElement /\ Scene.SetState (State f))
useGridScene args = Hooks.do
  dragged /\ setDragged <- Hooks.useState false
  scene /\ setScene <- useScene $ sceneArgs { dragged, setDragged }
  Hooks.pure $
    scene (if dragged then "cursor-grabbing" else "cursor-pointer")
    /\
    setScene
  where
    args' :: HookArgs f ()
    args' = U.trim args

    props step =
      { playing: args.playing
      , rule: args.rule
      , step
      , speed: args.speed
      }

    sceneArgs { dragged, setDragged } =
      { id: "canvas"
      , width: args.width
      , height: args.height
      , fill: "#f5f5f5"
      , init: init args
      , props: props args.step
      , update: update $ args' >< { dragged, setDragged }
      , view: view args
      }

init :: forall f r. HookArgs f r -> State f
init args = State
  { buffer: 0.0
  , dragging: Nothing
  , game: args.game
  , origin: 0.0 /\ 0.0
  , zoom: 5.0
  }

update :: forall f
  . InteractiveAutomaton f
  => HookArgs f ( setDragged :: Dispatch Boolean, dragged :: Boolean )
  -> Props
  -> State f
  -> Scene.Message
  -> Effect (State f)
update args props state@(State s) = case _ of
  Scene.Tick (Milliseconds ms) | props.playing -> do
    let
      duration = ms / 1000.0
      stepsPerSecond = Int.floor $ Number.pow 10.0 (Int.toNumber props.speed / 50.0)
      newSteps = if props.playing then Int.toNumber stepsPerSecond * duration else 0.0
      accumulatedSteps = s.buffer + newSteps
      presentSteps = Int.floor accumulatedSteps
      buffer = max 0.0 (accumulatedSteps - Int.toNumber presentSteps)
      game = if presentSteps > 0 then Life.steps presentSteps props.rule s.game else s.game

    when (presentSteps > 0) $
      args.onStep $ props.step + presentSteps

    pure' s { buffer = buffer, game = game }
  Scene.Tick _ ->
    pure' s { buffer = 0.0 }
  Scene.MouseDown (MouseEvent e) -> do
    let offsetX /\ offsetY = offset args state
    pure' s
      { dragging = Just
          { startX: e.clientX - offsetX
          , startY: e.clientY - offsetY
          , offsetX
          , offsetY
          }
      }
  Scene.MouseMove (MouseEvent e) ->
    case s.dragging of
      Just d -> do
        let
          offsetX' = e.clientX - d.startX
          offsetY' = e.clientY - d.startY
          hypotenuse = Number.sqrt (Number.pow (offsetX' - d.offsetX) 2.0 + Number.pow (offsetY' - d.offsetY) 2.0)

        when (hypotenuse >= 2.0) $
          args.setDragged true

        if args.dragged then
          pure' s
            { origin =
                (numCols args state / 2.0 - offsetX' / s.zoom - 0.5) /\
                (numRows args state / 2.0 - offsetY' / s.zoom - 0.5)
            }
        else
          pure state
      Nothing ->
        pure state
  Scene.MouseUp (MouseEvent e) -> do
    if args.dragged then do
      args.setDragged false
      pure' s { dragging = Nothing }
    else do
      rect <- getBoundingClientRect e.target
      let
        offsetX /\ offsetY = offset args state
        x = e.clientX - rect.left - offsetX
        y = e.clientY - rect.top - offsetY
        col = Int.floor (x / s.zoom)
        row = Int.floor (y / s.zoom)
      pure' s { dragging = Nothing, game = Life.toggle row col s.game }
  Scene.Wheel we@(WheelEvent e) -> do
    preventDefault e
    stopPropagation e
    let
      zoomFactor = clamp 0.85 1.15 (1.0 - 0.015 * WE.deltaY we)
      zoom = max 1.0 $ min 50.0 (s.zoom * zoomFactor)
    pure' s { zoom = zoom }
  where
    pure' = pure <<< State

view :: forall f r. CellularAutomaton f => HookArgs f r -> State f -> Scene.Element
view args state@(State s) = fold
  [ Scene.Fragment $
      s.game # Life.toCells # Set.filter isVisible # Array.fromFoldable <#> \(row /\ col) ->
        Scene.Rect
          { position:
              { x: Int.toNumber col * s.zoom + offsetX
              , y: Int.toNumber row * s.zoom + offsetY
              }
          , height: s.zoom
          , width: s.zoom
          , fill: "#ff75aa"
          }
  , gridLineConfig # foldMap \config ->
      Scene.Fragment
      [ Scene.Fragment $
          (0 .. Int.ceil (numCols args state)) <#> \col ->
            Scene.Line
              { start: { x: colX config col, y: 0.0 }
              , end: { x: colX config col, y: Int.toNumber args.height }
              , stroke: config.stroke
              }
      , Scene.Fragment $
          (0 .. Int.ceil (numRows args state)) <#> \row ->
            Scene.Line
              { start: { x: 0.0, y: rowY config row }
              , end: { x: Int.toNumber args.width, y: rowY config row }
              , stroke: config.stroke
              }
      ]
  ]
  where
    offsetX /\ offsetY = offset args state

    bounds =
      { maxX: Int.ceil (numCols args state - offsetX / s.zoom)
      , maxY: Int.ceil (numRows args state - offsetY / s.zoom)
      , minX: Int.floor (-offsetX / s.zoom)
      , minY: Int.floor (-offsetY / s.zoom)
      }

    isVisible (r /\ c) =
      r <= maxY && r >= minY && c <= maxX && c >= minX
      where
        { maxX, maxY, minX, minY } = bounds

    colX config col = Int.toNumber col * s.zoom + config.adjustX
    rowY config row = Int.toNumber row * s.zoom + config.adjustY

    gridLineConfig :: Maybe _
    gridLineConfig = do
      guard (s.zoom >= 10.0)
      let
        opacity = min 255 ((Int.floor s.zoom - 10) * 2)
        hex = Int.toStringAs Int.hexadecimal opacity # U.padLeft 2 "0"
        stroke = "#575757" <> hex
        adjustX = Number.remainder offsetX s.zoom
        adjustY = Number.remainder offsetY s.zoom
      pure
        { stroke
        , adjustX
        , adjustY
        }

offset :: forall f r. HookArgs f r -> State f -> Number /\ Number
offset args state@(State { origin: originX /\ originY, zoom }) =
  (zoom * (numCols args state / 2.0 - originX - 0.5)) /\
  zoom * (numRows args state / 2.0 - originY - 0.5)

numRows :: forall f r. HookArgs f r -> State f -> Number
numRows args (State state) = Int.toNumber args.height / state.zoom

numCols :: forall f r. HookArgs f r -> State f -> Number
numCols args (State state) = Int.toNumber args.width / state.zoom
