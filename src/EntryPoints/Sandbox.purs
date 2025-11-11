module EntryPoints.Sandbox
  ( main
  )
  where

import Prelude

import Control.Alt ((<|>))
import Data.Codec as CO
import Data.Foldable (foldMap, for_)
import Data.Int as Int
import Data.Maybe (fromMaybe)
import Data.MediaType (MediaType(..))
import Data.Monoid (guard)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Elmish (ReactElement, Transition, Dispatch, (<|))
import Elmish.Boot (defaultMain)
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Elmish.Hooks ((=/>))
import Elmish.Hooks as Hooks
import Life.Components.GridScene (useGridScene)
import Life.Components.Header as Header
import Life.Components.LogSlider as LogSlider
import Life.Components.TagSelect as TagSelect
import Life.Game.Patterns as P
import Life.Hooks.UseBoundingBox (useBoundingBox)
import Life.Types.Codec (codec)
import Life.Types.Game.Engines.Optimized.Unbounded (Unbounded)
import Life.Types.Game.Life as Life
import Life.Types.Game.NamedRule (NamedRule)
import Life.Types.Game.NamedRule as NamedRule
import Life.Types.Game.RLE (RLE(..))
import Life.Types.Game.RLE as RLE
import Life.Types.Grid.Cell (Cell)
import Life.Types.Grid.Cell as C
import Life.Utils.HTML (RClipboardEvent(..))
import Life.Utils.HTML as HTML
import Record as Record
import Web.Clipboard.ClipboardEvent as CE
import Web.HTML.Event.DataTransfer as DT

type State =
  { playing :: Boolean
  , rule :: NamedRule
  , stepsPerSecond :: Number
  , step :: Int
  }

data Message
  = SelectRule NamedRule
  | SetStepsPerSecond Number
  | SetStep Int
  | TogglePlaying

type Game = Unbounded

type SharedArgs r =
  { playing :: Boolean
  , stepsPerSecond :: Number
  , rule :: NamedRule
  , step :: Int
  , onStep :: Dispatch Int
  , onRuleChange :: Dispatch NamedRule
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
  , stepsPerSecond: 10.0
  , step: 0
  }

update :: State -> Message -> Transition Message State
update state = case _ of
  SelectRule rule ->
    pure state { rule = rule }
  SetStep n ->
    pure state { step = n }
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
      , step: state.step
      , onStep: dispatch <<< SetStep
      , onRuleChange: dispatch <<< SelectRule
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
            , H.div_ "ms-2 d-flex" { style: H.css { maxWidth: "150px" } } $
                LogSlider.view ""
                  { min: 1.0
                  , max: 1000.0
                  , value: state.stepsPerSecond
                  , onChange: dispatch <<< SetStepsPerSecond
                  , id: "speed-input"
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
    HTML.div_ ""
      { onPaste: E.handleEffect \(RClipboardEvent e) ->
          unless args.playing $
            for_ (CE.clipboardData e) \dt -> do
              text <- DT.getData (MediaType "text/plain") dt
              loadRLEText setScene text
      }
    [ args.controls
        { next: do
            setScene \s -> s { game = Life.steps 1 args.rule s.game }
            args.onStep (args.step + 1)
        , reset: do
            setScene _ { game = Life.empty 0 0 }
            args.onStep 0
        , currentStep: args.step
        }
    , scene
    ]
  where
    hookArgs =
      { playing: args.playing
      , stepsPerSecond: args.stepsPerSecond
      , rule: args.rule
      , step: args.step
      , onStep: args.onStep
      , width: args.width
      , height: args.height
      , originColor: "#bad5ff50"
      , game: Life.fromCells@Game 0 0 gliderGunWithEater
      }

    loadRLEText setScene text =
      for_ (CO.decode codec text) $ loadRLE setScene

    loadRLE setScene (RLE rle) = do
      let
        rule = fromMaybe NamedRule.default $ CO.decode NamedRule.descriptorCodec =<< rle.rule
        topLeft = rle.bounds <#> \{ x, y } -> -(Int.floor (Int.toNumber y / 2.0)) /\ -(Int.floor (Int.toNumber x / 2.0))
      args.onRuleChange rule
      setScene _
        { game =
            RLE.toCells (RLE rle { topLeft = topLeft <|> rle.topLeft })
              # Set.fromFoldable
              # Life.fromCells 0 0
        }

gliderGunWithEater :: Set Cell
gliderGunWithEater = Set.fromFoldable $
  C.adjust (-71 /\ -66) P.gliderGun <>
  C.adjust (42 /\ 60) P.eater
