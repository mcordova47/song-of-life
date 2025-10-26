module EntryPoints.Rules where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Monoid (guard)
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
import Life.Components.TagSelect as TagSelect
import Life.Types.Game.Optimized.Unbounded (Unbounded)
import Life.Types.Life as Life
import Life.Types.NamedRule (NamedRule)
import Life.Types.NamedRule as NamedRule

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
  , H.div "container flex-grow-1 d-flex flex-column pt-4" $
      grid
        { life: Life.empty 200 200
        , rows: 200
        , cols: 200
        , playing: state.playing
        , rule: state.rule
        , stepsPerFrame: state.stepsPerFrame
        , framesPerSecond: state.framesPerSecond
        , controls: \{ step, reset, currentStep } ->
            H.div "d-inline-flex align-items-center mb-3"
            [ H.button_ "btn bg-salmon hover:bright text-white"
                { onClick: dispatch <| TogglePlaying }
                if state.playing then "Pause" else "Play"
            , guard (not state.playing) $
                H.button_ "btn btn-outline-theme ms-2"
                  { onClick: E.handleEffect step }
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
  { life :: Game Boolean
  , rows :: Int
  , cols :: Int
  , playing :: Boolean
  , rule :: NamedRule
  , stepsPerFrame :: Int
  , framesPerSecond :: Int
  , controls :: Controls -> ReactElement
  }

type Controls =
  { step :: Effect Unit
  , reset :: Effect Unit
  , currentStep :: Int
  }

grid :: Args -> ReactElement
grid { life, rows, cols, playing, rule, stepsPerFrame, framesPerSecond, controls } = Hooks.component Hooks.do
  let msPerFrame = 1000.0 / Int.toNumber framesPerSecond

  game /\ setGame <- Hooks.useState life
  step /\ setStep <- Hooks.useState 0

  Hooks.useEffect' { playing, step } \deps -> do
    when deps.playing do
      delay $ Milliseconds msPerFrame
      liftEffect $ setGame $ Life.steps stepsPerFrame rule game
      liftEffect $ setStep (deps.step + 1)

  Hooks.pure $
    H.fragment
    [ controls
        { step: do
            setGame $ Life.step rule game
            setStep (step + 1)
        , reset: do
            setGame $ Life.empty rows cols
            setStep 0
        , currentStep: step
        }
    , if playing then
        H.div "position-relative flex-grow-1 w-100 overflow-hidden border-lightblue" $
          Life.toCells game # Array.fromFoldable <#> \(row /\ col) ->
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
              Life.renderInteractive
                { life: game
                , rows
                , cols
                , renderRow: \{ content } ->
                    H.div_ "d-flex"
                      { style: H.css { lineHeight: 0 } }
                      content
                , renderCol: \{ living, onClick } ->
                    H.div "d-inline-block m-0" $
                      H.div_ ("d-inline-block border-bottom border-end bg-" <> if living then "salmon" else "light")
                        { style: H.css
                            { height: "0.3rem"
                            , width: "0.3rem"
                            , cursor: "pointer"
                            }
                        , onClick: setGame <| onClick
                        }
                        H.empty
                }
    ]
