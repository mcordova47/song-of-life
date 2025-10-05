module Main where

import Prelude

import Control.Alternative (guard)
import Data.Array (foldl, (..))
import Data.Array as Array
import Data.Foldable (for_)
import Data.Int as Int
import Data.Maybe (Maybe(..), isJust)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Elmish (Dispatch, ReactElement, Transition, forkVoid, forks, (<?|), (<|))
import Elmish.Boot (defaultMain)
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Music (Note, a4, major2nd, major3rd, major6th, octave, perfect4th, perfect5th, (<<), (>>))

main :: Effect Unit
main = defaultMain { def: { init, view, update }, elementId: "app" }

data Message
  = AutoStep
  | Beat (Array Note) Milliseconds
  | Stop
  | Play
  | Reset
  | SetSpeed Int
  | Step
  | ToggleCell Cell

type State =
  { livingCells :: Set Cell
  , play :: Maybe Int
  , speed :: Int
  }

type Cell = Int /\ Int

init :: Transition Message State
init = pure
  { livingCells: Set.empty
  , play: Nothing
  , speed: 5
  }

update :: State -> Message -> Transition Message State
update state = case _ of
  AutoStep | Just _ <- state.play -> do
    let livingCells = step state.livingCells
    autoStep livingCells
    pure state { livingCells = livingCells, play = Just (Array.length (measure livingCells) - 1) }
  AutoStep ->
    pure state
  Beat notes' dur -> do
    forkVoid $ liftEffect $ runEffectFn2 playNotes_ notes' dur
    pure state { play = inc state.play }
  Stop ->
    pure state { play = Nothing }
  Play -> do
    autoStep state.livingCells
    pure state { play = Just (-1) }
  Reset ->
    pure state { livingCells = Set.empty }
  SetSpeed speed ->
    pure state { speed = speed }
  Step ->
    pure state { livingCells = step state.livingCells }
  ToggleCell cell ->
    pure state { livingCells = Set.toggle cell state.livingCells }
  where
    autoStep cells =
      forks \{ dispatch } -> do
        let
          measure' = measure cells
          durationMs = duration / Int.toNumber state.speed / Int.toNumber (Array.length measure')
        for_ measure' \notes' -> do
          delay $ Milliseconds durationMs
          liftEffect $ dispatch $ Beat notes' $ Milliseconds durationMs
        liftEffect $ dispatch AutoStep

    measure cells =
      transpose grid
      <#> Array.filter (\cell -> Set.member cell cells)
      <#> map fst
      <#> Array.mapMaybe (Array.index notes)

    inc = case _ of
      Just n -> Just $ mod (n + 1) (Array.length $ measure state.livingCells)
      Nothing -> Nothing

view :: State -> Dispatch Message -> ReactElement
view state dispatch =
  H.div "container" $
    H.div "mt-3 mx-auto"
    [ gridView
    , H.div "mt-3"
      [ H.button_ "btn btn-primary"
          { onClick: dispatch <| (if isJust state.play then Stop else Play) }
          if isJust state.play then
            "Stop"
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
            H.div_ "d-inline-block m-0 p-1"
              { style: case state.play of
                  Just n | n == snd cell -> H.css { backgroundColor: "#ffedf4" }
                  _ -> H.css {}
              } $
              H.div_ ("d-inline-block bg-" <> if Set.member cell state.livingCells then "primary" else "light")
                { style: H.css
                    { width: 30
                    , height: 30
                    , cursor: "pointer"
                    }
                , onClick: dispatch <| ToggleCell cell
                }
                H.empty

grid :: Array (Array Cell)
grid =
  rows <#> \row -> cols <#> \col -> row /\ col
  where
    rows = 0 .. Array.length notes
    cols = 0 .. (beatsPerMeasure - 1)

root :: Note
root = a4 << octave

notes :: Array Note
notes = ((>>) root) <$>
  [ mempty
  , major3rd
  , perfect5th
  , major2nd
  , perfect4th
  , major6th
  , octave
  , octave <> major3rd
  , octave <> perfect5th
  , octave <> major2nd
  , octave <> perfect4th
  , octave <> major6th
  ]

duration :: Number
duration = 15_000.0

beatsPerMeasure :: Int
beatsPerMeasure = 16

transpose :: forall a. Array (Array a) -> Array (Array a)
transpose rows = case Array.head rows of
  Just row -> transpose' (Array.length row) rows
  Nothing -> []
  where
    transpose' n = Array.replicate n [] # Array.foldl \cols row ->
      Array.zipWith (<>) cols (row <#> Array.singleton)

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

    neighbors (row /\ col) = do
      row' <- [row - 1, row, row + 1]
      col' <- [col - 1, col, col + 1]
      guard $ (row' /\ col') /= (row /\ col)
      pure (row' /\ col')

foreign import playNotes_ :: EffectFn2 (Array Note) Milliseconds Unit
