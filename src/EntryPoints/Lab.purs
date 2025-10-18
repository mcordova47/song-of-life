module EntryPoints.Lab where

import Prelude

import Data.Array ((..))
import Data.Array as Array
import Data.Int as Int
import Data.Monoid (guard)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Elmish (Dispatch, ReactElement, Transition, (<?|), (<|))
import Elmish.Boot (defaultMain)
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Elmish.Hooks as Hooks
import Life.Components.Header as Header
import Life.Game.Unbounded as Game
import Life.Types.Cell (Cell)

type State =
  { playing :: Boolean
  , stepsPerFrame :: Int
  }

data Message
  = SetStepsPerFrame Int
  | TogglePlaying

main :: Effect Unit
main = defaultMain
  { def: { init, view, update }
  , elementId: "app"
  }

init :: Transition Message State
init = pure { playing: false, stepsPerFrame: 1 }

update :: State -> Message -> Transition Message State
update state = case _ of
  SetStepsPerFrame n ->
    pure state { stepsPerFrame = n }
  TogglePlaying ->
    pure state { playing = not state.playing }

view :: State -> Dispatch Message -> ReactElement
view state dispatch = H.div "d-flex flex-column vh-100 overflow-auto"
  [ Header.view
  , H.div "container flex-grow-1 d-flex flex-column pt-4" $
      grid
        { cells: Set.fromFoldable
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
        , rows: 200
        , cols: 200
        , playing: state.playing
        , stepsPerFrame: state.stepsPerFrame
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
            , H.input_ "form-range ms-2"
                { type: "range"
                , min: "1"
                , max: "100"
                , step: "1"
                , value: show state.stepsPerFrame
                , onChange: dispatch <?| map SetStepsPerFrame <<< Int.fromString <<< E.inputText
                , id: "speed-input"
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
  { cells :: Set Cell
  , rows :: Int
  , cols :: Int
  , playing :: Boolean
  , stepsPerFrame :: Int
  , controls :: Controls -> ReactElement
  }

type Controls =
  { stepBy :: Dispatch Int
  , reset :: Effect Unit
  , currentStep :: Int
  }

grid :: Args -> ReactElement
grid { cells, rows, cols, playing, stepsPerFrame, controls } = Hooks.component Hooks.do
  let
    framesPerSecond = 60.0
    msPerFrame = 1000.0 / framesPerSecond
  livingCells /\ setLivingCells <- Hooks.useState cells
  step /\ setStep <- Hooks.useState 0

  Hooks.useEffect' { playing, step } \deps -> do
    when deps.playing do
      delay $ Milliseconds msPerFrame
      liftEffect $ setLivingCells $ Game.steps stepsPerFrame livingCells
      liftEffect $ setStep (deps.step + stepsPerFrame)

  let toggleCell cell = (if Set.member cell livingCells then Set.delete else Set.insert) cell livingCells

  Hooks.pure $
    H.fragment
    [ controls
        { stepBy: \n -> do
            setLivingCells $ Game.steps n livingCells
            setStep $ step + 1
        , reset: do
            setLivingCells Set.empty
            setStep 0
        , currentStep: step
        }
    , if playing then
        H.div "position-relative flex-grow-1 w-100 overflow-hidden border-lightblue" $
          livingCells # Array.fromFoldable <#> \(row /\ col) ->
            H.div_ "bg-salmon position-absolute"
              { style: H.css
                  { top: show (Int.toNumber row * 0.3) <> "rem"
                  , left: show (Int.toNumber col * 0.3) <> "rem"
                  , height: "0.3rem"
                  , width: "0.3rem"
                  }
              }
              H.empty
      else
        H.div "" $
          H.div "d-flex" $
            H.div "" $
              (0 .. (rows - 1)) <#> \row ->
                H.div_ "d-flex"
                  { style: H.css { lineHeight: 0 } } $
                  (0 .. (cols - 1)) <#> \col ->
                    H.div "d-inline-block m-0" $
                      H.div_ ("d-inline-block border-bottom border-end bg-" <> if Set.member (row /\ col) livingCells then "salmon" else "light")
                        { style: H.css
                            { height: "0.3rem"
                            , width: "0.3rem"
                            , cursor: "pointer"
                            }
                        , onClick: setLivingCells <| toggleCell (row /\ col)
                        }
                        H.empty
    ]
