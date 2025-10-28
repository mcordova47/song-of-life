module EntryPoints.Lab where

import Prelude

import Data.Array as Array
import Data.Foldable (fold, for_)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Effect (Effect, foreachE)
import Effect.Aff (Milliseconds(..), delay)
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
import Web.HTML.Window (document)

type State =
  { framesPerSecond :: Int
  , playing :: Boolean
  , rule :: NamedRule
  , stepsPerFrame :: Int
  }

data Message
  = SelectRule NamedRule
  | SetFramesPerSecond Int
  | SetStepsPerFrame Int
  | TogglePlaying

type Game = Unbounded

main :: Effect Unit
main = defaultMain
  { def: { init, view, update }
  , elementId: "app"
  }

init :: Transition Message State
init = pure
  { framesPerSecond: 10
  , playing: false
  , rule: NamedRule.default
  , stepsPerFrame: 1
  }

update :: State -> Message -> Transition Message State
update state = case _ of
  SelectRule rule ->
    pure state { rule = rule }
  SetFramesPerSecond n ->
    pure state { framesPerSecond = n }
  SetStepsPerFrame n ->
    pure state { stepsPerFrame = n }
  TogglePlaying ->
    pure state { playing = not state.playing }

view :: State -> Dispatch Message -> ReactElement
view state dispatch = H.div "d-flex flex-column vh-100 overflow-auto"
  [ Header.view
  , gridContainer
      { playing: state.playing
      , stepsPerFrame: state.stepsPerFrame
      , framesPerSecond: state.framesPerSecond
      , rule: state.rule
      , controls: \{ stepBy, reset, currentStep } ->
          H.div "container pt-3" $
            H.div "d-inline-flex align-items-center mb-3"
            [ H.button_ "btn bg-salmon hover:bright text-white"
                { onClick: dispatch <| TogglePlaying }
                if state.playing then "Pause" else "Play"
            , guard (not state.playing) $
                H.button_ "btn btn-outline-theme ms-2"
                  { onClick: stepBy <| 1 }
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
            , H.div "ms-2" "Step by:"
            , H.input_ "form-range ms-2"
                { type: "range"
                , min: "1"
                , max: "100"
                , step: "1"
                , value: show state.stepsPerFrame
                , onChange: dispatch <?| map SetStepsPerFrame <<< Int.fromString <<< E.inputText
                , id: "steps-input"
                , style: H.css { maxWidth: "150px" }
                }
            , H.div "ms-2" "FPS:"
            , H.input_ "form-range ms-2"
                { type: "range"
                , min: "1"
                , max: "60"
                , step: "1"
                , value: show state.framesPerSecond
                , onChange: dispatch <?| map SetFramesPerSecond <<< Int.fromString <<< E.inputText
                , id: "fps-input"
                , style: H.css { maxWidth: "150px" }
                }
            , H.div "d-flex align-items-center ms-2"
              [ H.div "" "Step #"
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
  , stepsPerFrame :: Int
  , framesPerSecond :: Int
  , rule :: NamedRule
  , controls :: Controls -> ReactElement
  | r
  }

type Size =
  ( width :: Int
  , height :: Int
  )

type Controls =
  { stepBy :: Dispatch Int
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

-- TODO:
-- - [ ] requestAnimationFrame, playing :: Maybe DateTime, zooming / steps / anything that triggers a redraw gets handled at the same scheduled ticks
-- - [ ] Diff cells and only redraw changes
-- - [x] Canvas 100% of parent element
-- - [ ] Grid lines at large zoom?
-- - [x] Origin in center, zooming keeps origin the same
-- - [x] Fix jittering when zooming
-- - [ ] Pan by dragging
grid :: Args Size -> ReactElement
grid { width, height, controls, playing, stepsPerFrame, framesPerSecond, rule } = Hooks.component Hooks.do
  zoom /\ setZoom <- Hooks.useState 5.0
  origin /\ _ <- Hooks.useState (0 /\ 0)

  let
    gridSize = 0 -- Number of cells doesn't matter for unbounded grid
    msPerFrame = 1000.0 / Int.toNumber framesPerSecond
    cellSize = zoom
    numRows = Int.toNumber height / cellSize
    numCols = Int.toNumber width / cellSize
    originX /\ originY = origin
    offsetX = cellSize * (numCols / 2.0 - 0.5)
    offsetY = cellSize * (numRows / 2.0 - 0.5)
    minX = originX - Int.ceil offsetX
    minY = originY - Int.ceil offsetY
    maxX = originX + Int.ceil offsetX
    maxY = originY + Int.ceil offsetY
    isVisible (r /\ c) =
      r <= maxY && r >= minY && c <= maxX && c >= minX

  game /\ setGame <- Hooks.useState $ Life.fromCells@Game gridSize gridSize gliderGunWithEater
  step /\ setStep <- Hooks.useState 0
  renderId /\ setRenderId <- Hooks.useState 0

  let
    zoomBy delta zr = do
      let factor = clamp 0.85 1.15 (1.0 - 0.015 * Int.toNumber delta)
      z <- Ref.read zr
      let zoom' = max 1.0 $ min 50.0 (z * factor)
      Ref.write zoom' zr
      setZoom zoom'

  Hooks.useEffect $ liftEffect do
    z <- Ref.new zoom

    mCanvas <- window >>= document <#> Doc.toNonElementParentNode >>= getElementById "canvas"
    for_ mCanvas \canvas -> do
      listener <- eventListener \e -> do
        preventDefault e
        stopPropagation e
        zoomBy (unsafeCoerce e).deltaY z

      canvas # toEventTarget # addEventListenerWithOptions (EventType "wheel") listener { capture: false, once: false, passive: false }

  Hooks.useEffect' { playing, renderId, zoom } \{ playing: playing' } -> do
    liftEffect do
      mCanvasElem <- C.getCanvasElementById "canvas"
      for_ mCanvasElem \canvasElem -> do
        ctx <- C.getContext2D canvasElem
        C.clearRect ctx { x: 0.0, y: 0.0, height: Int.toNumber height, width: Int.toNumber width }

        C.setFillStyle ctx "#f5f5f5"
        C.fillRect ctx { x: 0.0, y: 0.0, height: Int.toNumber height, width: Int.toNumber width }

        C.setFillStyle ctx "#ff75aa"

        foreachE (Life.toCells game # Set.filter isVisible # Array.fromFoldable) \(row /\ col) -> do
          let
            x = Int.toNumber col * cellSize + offsetX
            y = Int.toNumber row * cellSize + offsetY
          C.fillRect ctx { x, y, height: cellSize, width: cellSize }

    when playing' do
      delay $ Milliseconds msPerFrame
      liftEffect do
        setGame $ Life.steps stepsPerFrame rule game
        setStep $ step + stepsPerFrame
        setRenderId (renderId + 1)

  Hooks.pure $
    H.fragment
    [ controls
        { stepBy: \n -> do
            setGame $ Life.steps n rule game
            setStep (step + 1)
            setRenderId (renderId + 1)
        , reset: do
            setGame $ Life.empty gridSize gridSize
            setStep 0
            setRenderId (renderId + 1)
        , currentStep: step
        }
    , H.canvas_ "cursor-pointer"
        { id: "canvas"
        , width: show width <> "px"
        , height: show height <> "px"
        , style: H.css { width, height }
        , onClick: unsafeCoerce $ E.handleEffect \(MouseEvent e) -> do
            rect <- getBoundingClientRect e.target
            let
              x = e.clientX - rect.left - offsetX
              y = e.clientY - rect.top - offsetY
              col = Int.floor (x / cellSize)
              row = Int.floor (y / cellSize)
            setGame $ Life.toggle row col game
            setRenderId (renderId + 1)
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
