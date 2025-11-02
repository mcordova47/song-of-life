module EntryPoints.Sandbox where

import Prelude

import Data.Foldable (foldMap)
import Data.Int as Int
import Data.Monoid (guard)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Elmish (Dispatch, ReactElement, Transition, (<?|), (<|))
import Elmish.Boot (defaultMain)
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Elmish.Hooks ((=/>))
import Elmish.Hooks as Hooks
import Life.Components.GridScene (useGridScene)
import Life.Components.GridScene as GridScene
import Life.Components.Header as Header
import Life.Components.TagSelect as TagSelect
import Life.Hooks.UseBoundingBox (useBoundingBox)
import Life.Types.Game.Engines.Optimized.Unbounded (Unbounded)
import Life.Types.Game.Life as Life
import Life.Types.Game.NamedRule (NamedRule)
import Life.Types.Game.NamedRule as NamedRule
import Life.Types.Grid.Cell (Cell)
import Record as Record

type State =
  { playing :: Boolean
  , rule :: NamedRule
  , speed :: Int
  , step :: Int
  }

data Message
  = SelectRule NamedRule
  | SetSpeed Int
  | SetStep Int
  | TogglePlaying

type Game = Unbounded

type SharedArgs r =
  { playing :: Boolean
  , speed :: Int
  , rule :: NamedRule
  , step :: Int
  , onStep :: Dispatch Int
  , controls :: Controls -> ReactElement
  | r
  }

type GridContainerArgs = SharedArgs ()

type GridArgs = SharedArgs ( width :: Int, height :: Int )

type Controls =
  { next :: Effect Unit
  , reset :: Effect Unit
  , currentStep :: Int
  }

main :: Effect Unit
main = defaultMain
  { def: { init, view, update }
  , elementId: "app"
  }

init :: Transition Message State
init = pure
  { playing: false
  , rule: NamedRule.default
  , speed: 50
  , step: 0
  }

update :: State -> Message -> Transition Message State
update state = case _ of
  SelectRule rule ->
    pure state { rule = rule }
  SetSpeed n ->
    pure state { speed = n }
  SetStep n ->
    pure state { step = n }
  TogglePlaying ->
    pure state { playing = not state.playing }

view :: State -> Dispatch Message -> ReactElement
view state dispatch = H.div "d-flex flex-column vh-100 overflow-auto"
  [ Header.view
  , gridContainer
      { playing: state.playing
      , speed: state.speed
      , rule: state.rule
      , step: state.step
      , onStep: dispatch <<< SetStep
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
                , max: "150"
                , step: "1"
                , value: show state.speed
                , onChange: dispatch <?| map SetSpeed <<< Int.fromString <<< E.inputText
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

gridContainer :: GridContainerArgs -> ReactElement
gridContainer args = Hooks.component Hooks.do
  box /\ ref <- useBoundingBox
  let size = box <#> \b -> { width: Int.floor b.width, height: Int.floor b.height }
  Hooks.pure $
    H.div_ "flex-grow-1"
      { ref } $
      foldMap (grid <<< Record.merge args) size

grid :: GridArgs -> ReactElement
grid args =
  useGridScene hookArgs =/> \scene setScene ->
    H.fragment
    [ args.controls
        { next: do
            setScene \(GridScene.State s) -> GridScene.State s { game = Life.steps 1 args.rule s.game }
            args.onStep (args.step + 1)
        , reset: do
            setScene \(GridScene.State s) -> GridScene.State s { game = Life.empty 0 0 }
            args.onStep 0
        , currentStep: args.step
        }
    , scene
    ]
  where
    hookArgs =
      { playing: args.playing
      , speed: args.speed
      , rule: args.rule
      , step: args.step
      , onStep: args.onStep
      , width: args.width
      , height: args.height
      , originColor: "#bad5ff50"
      , game: Life.fromCells@Game 0 0 gliderGunWithEater
      }

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
