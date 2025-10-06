module Main where

import Prelude

import Control.Alternative (guard)
import Data.Array (foldl, (..))
import Data.Array as Array
import Data.Foldable (for_)
import Data.Int as Int
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid as M
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Elmish (Dispatch, ReactElement, Transition, forkVoid, forks, (<?|), (<|))
import Elmish.Boot (defaultMain)
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Life.Cell (Cell)
import Life.Icons as I
import Life.Music (Note, a4, major2nd, major3rd, major6th, octave, perfect4th, perfect5th, (<<), (>>))
import Life.Presets as Presets

main :: Effect Unit
main = defaultMain { def: { init, view, update }, elementId: "app" }

data Message
  = AutoStep
  | Beat (Array Note) Milliseconds
  | LoadPreset (Set Cell)
  | Pause
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

init :: Transition Message State
init = pure
  { livingCells: Presets.heart
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
  Pause ->
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
  LoadPreset cells ->
    pure state { livingCells = cells }
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
view state dispatch = H.fragment
  [ H.div "w-100 bg-lightblue" $
      H.div "container d-flex justify-content-between align-items-center py-2"
      [ H.h1 "d-inline-block mb-0" $
          H.a_ "text-salmon hover:text-salmon-highlight text-decoration-none"
            { href: "/" }
            "Songs of Life"
      , H.a_ "hover:translucent"
          { href: "https://github.com/mcordova47/song-of-life", target: "_blank" } $
          I.github { size: 48 }
      ]
  , H.div_ "container" { style: H.css { maxWidth: "800px" } } $
      H.div "mt-3 mx-auto"
      [ H.p ""
        [ H.text "Click some cells to change the starting conditions, then press play and "
        , H.a_ ""
            { href: "https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life"
            , target: "_blank"
            }
            [ H.text "Conway’s Game of Life "
            , I.externalLink { size: 16 }
            ]
        , H.text """
            will play out. Each row corresponds to a note and each column is a
            beat in a measure. Each beat will play and then the living cells
            will change and the next measure will play.
        """
        ]
      , gridView true state.livingCells
      , H.div "mt-3"
        [ H.div "d-flex" $
            H.div "d-inline-flex mx-auto bg-lightblue rounded-pill py-2 px-4"
            [ H.button_ "btn text-salmon hover:text-salmon-highlight p-0"
                { onClick: dispatch <| if isJust state.play then Pause else Play
                , title: if isJust state.play then "Pause" else "Play"
                }
                if isJust state.play then
                  I.pause { size: 32 }
                else
                  I.play { size: 32 }
              , H.button_ "btn text-salmon hover:text-salmon-highlight ms-2 p-0"
                  { onClick: dispatch <| Step
                  , title: "Step"
                  } $
                  I.arrowBarRight { size: 32 }
              , H.button_ "btn text-salmon hover:text-salmon-highlight ms-2 p-0"
                  { onClick: dispatch <| Reset
                  , title: "Reset"
                  } $
                  I.trash { size: 32 }
              ]
        , H.div "mt-3"
          [ H.h5 "" "Configuration"
          , H.div_ ""
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
        , H.div "mt-3"
          [ H.h5 "" "Presets"
          , presets
          ]
        ]
      ]
  ]
  where
    gridView full cells = H.div ("d-flex flex-column align-items-center mx-auto overflow-auto" <> M.guard (isJust state.play) " playing") $
      grid <#> \row ->
        H.div_ "d-flex"
          { style: H.css { lineHeight: 0 } } $
          row <#> \cell ->
            H.div ("d-inline-block m-0 grid-cell-container" <> M.guard (full && state.play == Just (snd cell)) " active") $
              H.div_ ("d-inline-block grid-cell bg-" <> if Set.member cell cells then "salmon" else "light")
                { onClick: dispatch' $ ToggleCell cell }
                H.empty
      where
        dispatch' msg =
          dispatch <?| if full then Just msg else Nothing

    presets =
      H.div "row" $ Presets.all <#> \cells ->
        H.div "col-6 col-sm-4 col-md-3 pb-3" $
          H.div_ "preset-grid d-flex rounded overflow-hidden border"
            { onClick: dispatch <| LoadPreset cells } $
            H.div "mx-auto" $
              grid <#> \row ->
                H.div_ "d-flex"
                  { style: H.css { lineHeight: 0 } } $
                  row <#> \cell ->
                    H.div "d-inline-block m-0 preset-grid-cell-container" $
                      H.div ("d-inline-block preset-grid-cell bg-" <> if Set.member cell cells then "salmon" else "light")
                        H.empty

grid :: Array (Array Cell)
grid =
  rows <#> \row -> cols <#> \col -> row /\ col
  where
    rows = 0 .. (Array.length notes - 1)
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
