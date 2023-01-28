module Main where

import Prelude

import Data.Array (foldl, (..))
import Data.Int as Int
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Elmish (Dispatch, ReactElement, Transition, fork, (<?|), (<|))
import Elmish.Boot (defaultMain)
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H

main :: Effect Unit
main = defaultMain { def: { init, view, update }, elementId: "app" }

-- Nothing happens in our UI so far, so there are no messages
data Message
  = AutoStep
  | Pause
  | Play
  | Reset
  | SetSpeed Int
  | Step
  | ToggleCell Cell

type State =
  { livingCells :: Set Cell
  , play :: Boolean
  , speed :: Int
  }

type Cell = Int /\ Int

init :: Transition Message State
init = pure
  { livingCells: Set.empty
  , play: false
  , speed: 5
  }

update :: State -> Message -> Transition Message State
update state = case _ of
  AutoStep | state.play -> do
    fork do
      delay $ Milliseconds $ 1000.0 / Int.toNumber state.speed
      pure AutoStep
    pure state { livingCells = step state.livingCells }
  AutoStep ->
    pure state
  Pause ->
    pure state { play = false }
  Play -> do
    fork $ pure AutoStep
    pure state { play = true }
  Reset ->
    pure state { livingCells = Set.empty :: _ Cell }
  SetSpeed speed ->
    pure state { speed = speed }
  Step ->
    pure state { livingCells = step state.livingCells }
  ToggleCell cell ->
    pure state { livingCells = Set.toggle cell state.livingCells }

view :: State -> Dispatch Message -> ReactElement
view state dispatch =
  H.div "container" $
    H.div "mt-3 mx-auto"
    [ gridView
    , H.div "mt-3"
      [ H.button_ "btn btn-primary"
          { onClick: dispatch <| (if state.play then Pause else Play) }
          if state.play then
            "Pause"
          else
            "Play"
      , H.button_ "btn btn-outline-primary ms-2"
          { onClick: dispatch <| Step }
          "Step"
      , H.button_ "btn btn-outline-primary ms-2"
          { onClick: dispatch <| Reset }
          "Reset"
      , H.div_ "mt-3"
        { style: H.css { width: 200 }
        }
        [ H.label_ "form-label fw-bold"
            { htmlFor: "speed-input" }
            "Speed"
        , H.input_ "form-range"
            { type: "range"
            , min: "1"
            , max: "10"
            , step: "1"
            , value: show state.speed
            , onChange: dispatch <?| map SetSpeed <<< Int.fromString <<< E.inputText
            , id: "speed-input"
            }
        ]
      ]
    ]
  where
    gridView = H.div "" $
      grid <#> \row ->
        H.div "" $
          row <#> \cell ->
            H.div_ ("d-inline-block bg-" <> if Set.member cell state.livingCells then "primary" else "light")
              { style: H.css { width: 15, height: 15, cursor: "pointer", margin: "0.1rem" }
              , onClick: dispatch <| ToggleCell cell
              }
              H.empty

grid :: Array (Array Cell)
grid =
  cellRange <#> \row -> cellRange <#> \col -> row /\ col
  where
    cellRange = 0 .. 29

step :: Set Cell -> Set Cell
step livingCells = foldl stepRow livingCells grid
  where
    stepRow livingCells' row =
      foldl stepCell livingCells' row

    stepCell livingCells' cell =
      case livingNeighbors cell of
        3 -> Set.insert cell livingCells'
        2 -> livingCells'
        _ -> Set.delete cell livingCells'

    livingNeighbors cell =
      foldl countLiving 0 $ neighbors cell

    countLiving acc cell =
      if Set.member cell livingCells then acc + 1 else acc

    neighbors (row /\ col) =
      [ (row - 1) /\ (col - 1)
      , (row - 1) /\ col
      , (row - 1) /\ (col + 1)
      , row /\ (col - 1)
      , row /\ (col + 1)
      , (row + 1) /\ (col - 1)
      , (row + 1) /\ col
      , (row + 1) /\ (col + 1)
      ]
