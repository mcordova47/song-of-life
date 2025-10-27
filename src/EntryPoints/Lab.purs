module EntryPoints.Lab where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Int as Int
import Data.Monoid (guard)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Effect (Effect, foreachE)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Elmish (Dispatch, ReactElement, Transition, (<?|), (<|))
import Elmish.Boot (defaultMain)
import Elmish.Foreign (class CanReceiveFromJavaScript)
import Elmish.HTML as HH
import Elmish.HTML.Events (EventHandler, MouseEvent(..))
import Elmish.HTML.Events as E
import Elmish.HTML.Events.Methods (class IsSyntheticEvent)
import Elmish.HTML.Internal as I
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
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (getBoundingClientRect)

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
view state dispatch = H.div "d-flex flex-column vh-100 overflow-hidden"
  [ Header.view
  , H.div "container flex-grow-1 d-flex flex-column pt-4" $
      grid
        { width: 1000
        , height: 1000
        , playing: state.playing
        , stepsPerFrame: state.stepsPerFrame
        , framesPerSecond: state.framesPerSecond
        , rule: state.rule
        , controls: \{ stepBy, reset, currentStep } ->
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
  ]

type Args =
  { width :: Int
  , height :: Int
  , playing :: Boolean
  , stepsPerFrame :: Int
  , framesPerSecond :: Int
  , rule :: NamedRule
  , controls :: Controls -> ReactElement
  }

type Controls =
  { stepBy :: Dispatch Int
  , reset :: Effect Unit
  , currentStep :: Int
  }

grid :: Args -> ReactElement
grid { width, height, controls, playing, stepsPerFrame, framesPerSecond, rule } = Hooks.component Hooks.do
  zoom /\ setZoom <- Hooks.useState 5.0

  let
    gridSize = 200
    cellSize = zoom
    msPerFrame = 1000.0 / Int.toNumber framesPerSecond
    numRows = Int.ceil (Int.toNumber height / cellSize)
    numCols = Int.ceil (Int.toNumber width / cellSize)
    isVisible (r /\ c) =
      r <= numRows && r >= 0 && c <= numCols && c >= 0

  game /\ setGame <- Hooks.useState $ Life.fromCells@Game gridSize gridSize gliderGunWithEater
  step /\ setStep <- Hooks.useState 0
  renderId /\ setRenderId <- Hooks.useState 0

  Hooks.useEffect' { playing, renderId } \{ playing: playing' } -> do
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
            x = Int.toNumber col * cellSize -- + pan.x
            y = Int.toNumber row * cellSize -- + pan.y
          C.fillRect ctx { x, y, height: cellSize - 1.0, width: cellSize - 1.0 }

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
    , canvas_ ""
        { id: "canvas"
        , width: show width <> "px"
        , height: show height <> "px"
        , style: H.css { width, height }
        , onClick: unsafeCoerce $ E.handleEffect \(MouseEvent e) -> do
            rect <- getBoundingClientRect e.target
            let
              x = e.clientX - rect.left -- - pan.x
              y = e.clientY - rect.top -- - pan.y
              col = Int.floor (x / cellSize)
              row = Int.floor (y / cellSize)
            setGame $ Life.toggle row col game
            setRenderId (renderId + 1)
        , onWheel: E.handleEffect \e@(WheelEvent ev) -> do
            E.preventDefault e
            E.stopPropagation e
            let
              delta = if ev.deltaY > 0 then 0.99 else 1.01

            setZoom $ max 1.0 (min 50.0 (zoom * delta))
            setRenderId $ (renderId + 1)
        }
        H.empty
    ]

newtype WheelEvent = WheelEvent { deltaY :: Int }
derive newtype instance CanReceiveFromJavaScript WheelEvent
instance IsSyntheticEvent WheelEvent

canvas_ :: I.StyledTag_ ( onWheel :: EventHandler WheelEvent | HH.Props_canvas )
canvas_ = I.styledTag_ "canvas"

gliderGunWithEater :: Set Cell
gliderGunWithEater = Set.fromFoldable
  [ 4 /\ 33
  , 5 /\ 31
  , 5 /\ 33
  , 6 /\ 21
  , 6 /\ 22
  , 6 /\ 29
  , 6 /\ 30
  , 6 /\ 43
  , 6 /\ 44
  , 7 /\ 20
  , 7 /\ 24
  , 7 /\ 29
  , 7 /\ 30
  , 7 /\ 43
  , 7 /\ 44
  , 8 /\ 9
  , 8 /\ 10
  , 8 /\ 19
  , 8 /\ 25
  , 8 /\ 29
  , 8 /\ 30
  , 9 /\ 9
  , 9 /\ 10
  , 9 /\ 19
  , 9 /\ 23
  , 9 /\ 25
  , 9 /\ 26
  , 9 /\ 31
  , 9 /\ 33
  , 10 /\ 19
  , 10 /\ 25
  , 10 /\ 33
  , 11 /\ 20
  , 11 /\ 24
  , 12 /\ 21
  , 12 /\ 22
  , 117 /\ 135
  , 117 /\ 136
  , 118 /\ 135
  , 119 /\ 136
  , 119 /\ 137
  , 119 /\ 138
  , 120 /\ 138
  ]
