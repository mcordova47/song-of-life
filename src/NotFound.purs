module NotFound where

import Prelude

import Data.Array.NonEmpty as NA
import Data.Foldable (foldMap)
import Data.Int as Int
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Elmish (ReactElement)
import Elmish.Boot (defaultMain)
import Elmish.HTML.Styled as H
import Elmish.Hooks as Hooks
import Life.Components.GridScene (State(..), useGridScene)
import Life.Components.Header as Header
import Life.Hooks.UseBoundingBox (useBoundingBox)
import Life.NotFound.Mazes (mazes)
import Life.Types.Game.Optimized.Unbounded (Unbounded)
import Life.Types.NamedRule as NamedRule

type Game = Unbounded

main :: Effect Unit
main = defaultMain
  { def: { init: pure unit, view: const $ const view, update: const absurd }
  , elementId: "app"
  }

view :: ReactElement
view = H.div "d-flex flex-column vh-100 overflow-auto"
  [ Header.view
  , gridContainer
  , H.style "" """
      body { padding-bottom: 0 !important; }
    """
  ]

gridContainer :: ReactElement
gridContainer = Hooks.component Hooks.do
  box /\ ref <- useBoundingBox
  let size = box <#> \b -> { width: Int.floor b.width, height: Int.floor b.height }
  Hooks.pure $
    H.div_ "flex-grow-1"
      { ref } $
      foldMap grid size

grid :: { height :: Int, width :: Int } -> ReactElement
grid { width, height } = Hooks.component Hooks.do
  step /\ setStep <- Hooks.useState 0
  games /\ setGames <- Hooks.useState (mazes @Game)
  scene /\ setScene <- useGridScene $ args (NA.head mazes) step setStep

  Hooks.useEffect' step \_ -> do
    when (step >= maxStep) $ liftEffect do
      let
        { head, tail } = NA.uncons games
        games' = NA.snoc' tail head
      setGames games'
      setScene \(State s) -> State s { game = NA.head games' }
      setStep 0

  Hooks.pure $
    H.div "position-relative h-100 w-100"
    [ scene
    , H.div_ "position-absolute"
        { style: H.css { top: "1rem", left: "50%", transform: "translateX(-50%)" }
        } $
          H.h2 "text-salmon" "Lost?"
    ]
  where
    maxStep = 350

    args game step onStep =
      { playing: step <= maxStep
      , speed: 75
      , rule: NamedRule.Maze
      , step
      , onStep
      , width
      , height
      , backgroundColor: "#ffffff"
      , cellColor: "#bad5ff70"
      , game
      }
