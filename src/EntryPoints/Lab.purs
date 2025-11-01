module EntryPoints.Lab where

import Prelude

import Data.Foldable (fold, for_)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Elmish (Dispatch, ReactElement, Transition, (<?|), (<|))
import Elmish.Boot (defaultMain)
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Elmish.Hooks as Hooks
import Life.Components.GridScene as GridScene
import Life.Components.Header as Header
import Life.Components.TagSelect as TagSelect
import Life.Types.Cell (Cell)
import Life.Types.Game.Optimized.Unbounded (Unbounded)
import Life.Types.Life as Life
import Life.Types.NamedRule (NamedRule)
import Life.Types.NamedRule as NamedRule
import Life.Utils (Opaque(..))
import Record as Record
import Web.DOM.Element (getBoundingClientRect)
import Web.HTML.HTMLDivElement as Div

type State =
  { playing :: Boolean
  , rule :: NamedRule
  , speed :: Int
  }

data Message
  = SelectRule NamedRule
  | SetSpeed Int
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
  , speed: 50
  }

update :: State -> Message -> Transition Message State
update state = case _ of
  SelectRule rule ->
    pure state { rule = rule }
  SetSpeed n ->
    pure state { speed = n }
  TogglePlaying ->
    pure state { playing = not state.playing }

view :: State -> Dispatch Message -> ReactElement
view state dispatch = H.div "d-flex flex-column vh-100 overflow-auto"
  [ Header.view
  , gridContainer
      { playing: state.playing
      , speed: state.speed
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

gridContainer :: GridScene.Args () -> ReactElement
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
        pure $ GridScene.component $ Record.merge args $ Record.merge size' { game: Life.fromCells@Game 0 0 gliderGunWithEater }

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
