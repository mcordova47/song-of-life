module EntryPoints.Lab where

import Prelude

import Data.Array as Array
import Data.Foldable (fold, for_)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Number (sqrt)
import Data.Number as Number
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Effect (Effect, foreachE)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Elmish (Dispatch, ReactElement, Transition, (<?|), (<|))
import Elmish.Boot (defaultMain)
import Elmish.HTML.Events (MouseEvent(..))
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Elmish.Hooks as Hooks
import Graphics.Canvas as C
import Life.Components.Header as Header
import Life.Components.TagSelect as TagSelect
import Life.Types.Cell (Cell)
import Life.Types.Game.Optimized.Unbounded (Unbounded)
import Life.Types.Life as Life
import Life.Types.NamedRule (NamedRule)
import Life.Types.NamedRule as NamedRule
import Life.Utils (Opaque(..))
import Record as Record
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (getBoundingClientRect, toEventTarget)
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event (EventType(..), preventDefault, stopPropagation)
import Web.Event.EventTarget (addEventListenerWithOptions, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDivElement as Div
import Web.HTML.HTMLDocument as Doc
import Web.HTML.Window (document, requestAnimationFrame)

type State =
  { playing :: Boolean
  , rule :: NamedRule
  , stepsPerSecond :: Int
  }

data Message
  = SelectRule NamedRule
  | SetStepsPerSecond Int
  | TogglePlaying

type Game = Unbounded

main :: Effect Unit
main = defaultMain
  { def: { init, view, update }
  , elementId: "app"
  }

init :: Transition Message State
init = pure
  { playing: false
  , rule: NamedRule.default
  , stepsPerSecond: 10
  }

update :: State -> Message -> Transition Message State
update state = case _ of
  SelectRule rule ->
    pure state { rule = rule }
  SetStepsPerSecond n ->
    pure state { stepsPerSecond = n }
  TogglePlaying ->
    pure state { playing = not state.playing }

view :: State -> Dispatch Message -> ReactElement
view state dispatch = H.div "d-flex flex-column vh-100 overflow-auto"
  [ Header.view
  , gridContainer
      { playing: state.playing
      , stepsPerSecond: state.stepsPerSecond
      , rule: state.rule
      , controls: \{ next, reset, currentStep } ->
          H.div "container pt-3" $
            H.div "d-inline-flex align-items-center mb-3"
            [ H.button_ "btn bg-salmon hover:bright text-white"
                { onClick: dispatch <| TogglePlaying }
                if state.playing then "Pause" else "Play"
            , guard (not state.playing) $
                H.button_ "btn btn-outline-theme ms-2"
                  { onClick: E.handleEffect next }
                  "Next"
            , guard (not state.playing) $
                H.button_ "btn btn-outline-theme ms-2"
                  { onClick: E.handleEffect reset }
                  "Reset"
            , H.div_ "ms-2"
                { style: H.css { maxWidth: "300px" }
                } $
                TagSelect.view
                  { display: NamedRule.display
                  , onChange: dispatch <<< SelectRule
                  , value: state.rule
                  }
            , H.div "ms-2" "Speed:"
            , H.input_ "form-range ms-2"
                { type: "range"
                , min: "1"
                , max: "100"
                , step: "1"
                , value: show state.stepsPerSecond
                , onChange: dispatch <?| map SetStepsPerSecond <<< Int.fromString <<< E.inputText
                , id: "speed-input"
                , style: H.css { maxWidth: "150px" }
                }
            , H.div "d-flex align-items-center ms-2"
              [ H.div "text-nowrap" "Step #"
              , H.div "h4 text-salmon ms-1 mb-0" $ show currentStep
              ]
            ]
      }
  , H.style "" """
      body { padding-bottom: 0 !important; }
    """
  ]

type Args r =
  { playing :: Boolean
  , stepsPerSecond :: Int
  , rule :: NamedRule
  , controls :: Controls -> ReactElement
  | r
  }

type Size =
  ( width :: Int
  , height :: Int
  )

type Controls =
  { next :: Effect Unit
  , reset :: Effect Unit
  , currentStep :: Int
  }

gridContainer :: Args () -> ReactElement
gridContainer args = Hooks.component Hooks.do
  size /\ setSize <- Hooks.useState Nothing
  elem /\ ref <- Hooks.useRef

  Hooks.useEffect' (Opaque <$> elem) \maybeEl -> liftEffect do
    for_ maybeEl \(Opaque el) -> do
      box <- el # Div.toElement # getBoundingClientRect
      setSize $ Just { width: Int.floor box.width, height: Int.floor box.height }

  Hooks.pure $
    H.div_ "flex-grow-1"
      { ref } $
      fold do
        size' <- size
        pure $ grid $ Record.merge args size'

-- TODO: Grid lines at large zoom?
grid :: Args Size -> ReactElement
grid { width, height, controls, playing, stepsPerSecond, rule } = Hooks.component Hooks.do
  zoom /\ setZoom <- Hooks.useState 5.0
  origin /\ setOrigin <- Hooks.useState (0.0 /\ 0.0)
  dragging /\ setDragging <- Hooks.useState Nothing
  dragged /\ setDragged <- Hooks.useState false
  tick /\ setTick <- Hooks.useState 0
  tickRef /\ setTickRef <- Hooks.useState Nothing
  step /\ setStep <- Hooks.useState 0

  let
    gridSize = 0 -- Number of cells doesn't matter for unbounded grid
    cellSize = zoom
    numRows = Int.toNumber height / cellSize
    numCols = Int.toNumber width / cellSize
    originX /\ originY = origin
    offsetX = cellSize * (numCols / 2.0 - originX - 0.5)
    offsetY = cellSize * (numRows / 2.0 - originY - 0.5)
    maxX = Int.ceil (numCols - offsetX / cellSize)
    maxY = Int.ceil (numRows - offsetY / cellSize)
    minX = Int.floor (-offsetX / cellSize)
    minY = Int.floor (-offsetY / cellSize)
    isVisible (r /\ c) =
      r <= maxY && r >= minY && c <= maxX && c >= minX

  game /\ setGame <- Hooks.useState $ Life.fromCells@Game gridSize gridSize gliderGunWithEater

  let
    zoomBy delta zr = do
      let factor = clamp 0.85 1.15 (1.0 - 0.015 * Int.toNumber delta)
      z <- Ref.read zr
      let zoom' = max 1.0 $ min 50.0 (z * factor)
      Ref.write zoom' zr
      setZoom zoom'

    blankSlate ctx = do
      C.clearRect ctx { x: 0.0, y: 0.0, height: Int.toNumber height, width: Int.toNumber width }
      C.setFillStyle ctx "#f5f5f5"
      C.fillRect ctx { x: 0.0, y: 0.0, height: Int.toNumber height, width: Int.toNumber width }

    drawCells ctx cells = do
      C.setFillStyle ctx "#ff75aa"
      foreachE (cells # Set.filter isVisible # Array.fromFoldable) \(row /\ col) -> do
        let
          x = Int.toNumber col * cellSize + offsetX
          y = Int.toNumber row * cellSize + offsetY
        C.fillRect ctx { x, y, height: cellSize, width: cellSize }

    repaint cells = liftEffect do
      mCanvasElem <- C.getCanvasElementById "canvas"
      for_ mCanvasElem \canvasElem -> do
        ctx <- C.getContext2D canvasElem
        blankSlate ctx
        drawCells ctx cells

    onTick = do
      setTick $ tick + 1

      for_ tickRef \ref -> do
        prevState <- Ref.read ref

        let
          duration = 1.0 / 60.0
          newSteps = if playing then Int.toNumber stepsPerSecond * duration else 0.0
          accumulatedSteps = prevState.buffer + newSteps
          presentSteps = Int.floor accumulatedSteps
          buffer = max 0.0 (accumulatedSteps - Int.toNumber presentSteps)
          game' = if presentSteps > 0 then Life.steps presentSteps rule game else game
          cells = Life.toCells game'

        Ref.write { buffer, origin, zoom, cells } ref

        when (presentSteps > 0) $
          setStep $ step + presentSteps

        when (cells /= prevState.cells || origin /= prevState.origin || zoom /= prevState.zoom) do
          repaint cells

          when (presentSteps > 0) $
            setGame game'

  Hooks.useEffect $ liftEffect do
    b <- Ref.new $ { buffer: 0.0, origin, zoom, cells: Set.empty }
    setTickRef $ Just b

    z <- Ref.new zoom

    mCanvas <- window >>= document <#> Doc.toNonElementParentNode >>= getElementById "canvas"
    for_ mCanvas \canvas -> do
      listener <- eventListener \e -> do
        preventDefault e
        stopPropagation e
        zoomBy (unsafeCoerce e).deltaY z

      canvas # toEventTarget # addEventListenerWithOptions (EventType "wheel") listener { capture: false, once: false, passive: false }

  Hooks.useEffect' tick \_ -> liftEffect $ void $
    window >>= requestAnimationFrame onTick

  Hooks.pure $
    H.fragment
    [ controls
        { next: do
            setGame $ Life.steps 1 rule game
            setStep (step + 1)
            for_ tickRef \ref -> do
              prevState <- Ref.read ref
              Ref.write prevState { buffer = prevState.buffer + 1.0 } ref
        , reset: do
            setGame $ Life.empty gridSize gridSize
            setStep 0
        , currentStep: step
        }
    , H.canvas_ (if dragged then "cursor-grabbing" else "cursor-pointer")
        { id: "canvas"
        , width: show width <> "px"
        , height: show height <> "px"
        , style: H.css { width, height }
        , onMouseDown: E.handleEffect \(MouseEvent e) -> do
            setDragging $ Just
              { startX: e.clientX - offsetX
              , startY: e.clientY - offsetY
              , offsetX
              , offsetY
              }
        , onMouseMove: E.handleEffect \(MouseEvent e) ->
            for_ dragging \d -> do
              let
                offsetX' = e.clientX - d.startX
                offsetY' = e.clientY - d.startY
                hypotenuse = sqrt (Number.pow (offsetX' - d.offsetX) 2.0 + Number.pow (offsetY' - d.offsetY) 2.0)

              when (hypotenuse >= 2.0) $
                setDragged true

              when dragged $
                setOrigin $
                  (numCols / 2.0 - offsetX' / cellSize - 0.5) /\
                  (numRows / 2.0 - offsetY' / cellSize - 0.5)
        , onMouseUp: E.handleEffect \(MouseEvent e) -> do
            unless dragged do
              rect <- getBoundingClientRect e.target
              let
                x = e.clientX - rect.left - offsetX
                y = e.clientY - rect.top - offsetY
                col = Int.floor (x / cellSize)
                row = Int.floor (y / cellSize)
              setGame $ Life.toggle row col game
            setDragging Nothing
            setDragged false
        }
        H.empty
    ]

gliderGunWithEater :: Set Cell
gliderGunWithEater = Set.fromFoldable
  [ -71 /\ -42
  , -70 /\ -44
  , -70 /\ -42
  , -69 /\ -54
  , -69 /\ -53
  , -69 /\ -46
  , -69 /\ -45
  , -69 /\ -32
  , -69 /\ -31
  , -68 /\ -55
  , -68 /\ -51
  , -68 /\ -46
  , -68 /\ -45
  , -68 /\ -32
  , -68 /\ -31
  , -67 /\ -66
  , -67 /\ -65
  , -67 /\ -56
  , -67 /\ -50
  , -67 /\ -46
  , -67 /\ -45
  , -66 /\ -66
  , -66 /\ -65
  , -66 /\ -56
  , -66 /\ -52
  , -66 /\ -50
  , -66 /\ -49
  , -66 /\ -44
  , -66 /\ -42
  , -65 /\ -56
  , -65 /\ -50
  , -65 /\ -42
  , -64 /\ -55
  , -64 /\ -51
  , -63 /\ -54
  , -63 /\ -53
  , 42 /\ 60
  , 42 /\ 61
  , 43 /\ 60
  , 44 /\ 61
  , 44 /\ 62
  , 44 /\ 63
  , 45 /\ 63
  ]
