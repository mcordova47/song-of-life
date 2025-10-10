module Main where

import Prelude

import Control.Alternative (guard)
import Data.Array (foldl, (..))
import Data.Array as Array
import Data.Foldable (fold, for_)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Monoid as M
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Elmish (Dispatch, ReactElement, Transition, fork, forkVoid, forks, (<?|), (<|))
import Elmish.Boot (defaultMain)
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Life.Icons as I
import Life.Music (Note, a4, major2nd, major3rd, major6th, octave, perfect4th, perfect5th, playNote, (<<), (>>))
import Life.Types.Cell (Cell)
import Life.Types.Preset as Preset
import Life.Wave (Wave)
import Life.Wave as Wave
import Web.HTML (window)
import Web.HTML.Location as Loc
import Web.HTML.Window (location)

main :: Effect Unit
main = defaultMain { def: { init, view, update }, elementId: "app" }

data Message
  = ApplyPreset String
  | AutoStep
  | Beat (Array Note) Milliseconds
  | LoadPreset (Set Cell)
  | Pause
  | Play
  | Reset
  | SetSpeed Int
  | SetShareInput String
  | SetWave Wave
  | ShowShareInput
  | Step
  | ToggleCell Cell

type State =
  { livingCells :: Set Cell
  , play :: Maybe Int
  , shareInput :: Maybe String
  , speed :: Int
  , wave :: Wave
  }

init :: Transition Message State
init = do
  fork $ liftEffect $
    window >>= location >>= Loc.hash <#> String.drop 2 <#> ApplyPreset
  pure
    { livingCells: Set.empty
    , play: Nothing
    , shareInput: Nothing
    , speed: 5
    , wave: Wave.default
    }

update :: State -> Message -> Transition Message State
update state = case _ of
  ApplyPreset hash ->
    let preset = Preset.decode hash # fromMaybe Preset.default
    in
    pure state
      { livingCells = Preset.livingCells preset
      , wave = Preset.wave preset
      }
  -- TODO: Refactor AutoStep logic:
  --  - length - 1 hack
  --  - let Beat drive the engine so that speed and notes can be changed in real time
  AutoStep | Just _ <- state.play -> do
    let livingCells = step state.livingCells
    autoStep livingCells
    pure state { livingCells = livingCells, play = Just (Array.length (measure livingCells) - 1) }
  AutoStep ->
    pure state
  Beat notes' dur -> do
    forkVoid $ liftEffect $ for_ notes' $ playNote dur state.wave
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
  SetShareInput origin ->
    pure state { shareInput = Just $ origin <> "/#/" <> (Preset.encode $ Preset.fromState state) }
  SetWave wave ->
    pure state { wave = wave }
  ShowShareInput -> do
    fork $ liftEffect $
      window >>= location >>= Loc.origin <#> SetShareInput
    pure state
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
      , gridView
      , H.div "mt-3"
        [ H.div "d-flex" $
            H.div "d-inline-flex align-items-center mx-auto bg-lightblue rounded-pill py-1 px-4"
            [ H.button_ "btn text-salmon hover:text-salmon-highlight p-0"
                  { onClick: dispatch <| Reset
                  , title: "Reset"
                  } $
                  I.trash { size: 32 }
            , H.button_ "btn text-salmon hover:text-salmon-highlight ms-3 me-2 p-0"
                { onClick: dispatch <| if isJust state.play then Pause else Play
                , title: if isJust state.play then "Pause" else "Play"
                }
                if isJust state.play then
                  I.pause { size: 64 }
                else
                  I.play { size: 64 }
              , H.button_ "btn text-salmon hover:text-salmon-highlight p-0"
                  { onClick: dispatch <| Step
                  , title: "Step"
                  } $
                  I.arrowBarRight { size: 32 }
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
          , H.label "mb-2" "Wave Type"
          , H.div "row" $ Wave.all <#> \wave ->
              H.div "col-6 col-sm-3 col-lg-2" $
                H.div_ ("border rounded card-btn mb-3" <> M.guard (wave == state.wave) " active")
                  { onClick: dispatch <| SetWave wave } $
                  H.div "mx-auto text-center"
                  [ H.div "" $
                      Wave.icon { size: 48 } wave
                  , H.div "" $ Wave.display wave
                  ]
          ]
        , H.div "mt-3"
          [ H.h5 "" "Rules"
          , H.p ""
            [ H.text "The "
            , H.strong "text-salmon" "Game of Life"
            , H.text """
                is often referred to as a zero-player game. Each
                step of the game is determined by the previous step and consists
                of changing the state of each of the cells.
              """
            ]
          , H.ol ""
            [ H.li ""
              [ H.text "A cell is either "
              , H.strong "text-salmon" "alive"
              , H.text " or "
              , H.strong "text-salmon" "dead"
              ]
            , H.li ""
              [ H.text "A cell’s "
              , H.strong "text-salmon" "neighbors"
              , H.text " are the cells adjacent to that cell (vertically, horizontally, or diagonally)"
              ]
            , H.li "" "The state of a given cell is determined by its neighbors’ previous state"
            , H.li ""
              [ H.text "A cell with "
              , H.strong "text-salmon" "fewer than 2"
              , H.text " living neighbors will die"
              ]
            , H.li ""
              [ H.text "A cell with "
              , H.strong "text-salmon" "2"
              , H.text " living neighbors will stay alive (or dead)"
              ]
            , H.li ""
              [ H.text "A cell with "
              , H.strong "text-salmon" "3"
              , H.text " living neighbors will come to life"
              ]
            , H.li ""
              [ H.text "A cell with "
              , H.strong "text-salmon" "more than 3"
              , H.text " living neighbors will die"
              ]
            ]
          , H.p ""
            [ H.text "This is a "
            , H.strong "text-salmon" "bounded"
            , H.text " Game of Life, whereas it is often played on an infinite grid."
            ]
          ]
        , H.div "mt-3"
          [ H.h5 "" "Share"
          , H.div "d-flex"
            [ H.button_ "btn btn-outline-theme"
                { onClick: dispatch <| ShowShareInput
                }
                "Share"
            , fold do
                value <- state.shareInput
                pure $
                  H.input_ "form-control"
                    { value
                    , readOnly: true
                    , autoFocus: true
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
    gridView = H.div ("d-flex flex-column align-items-center mx-auto overflow-auto" <> M.guard (isJust state.play) " playing") $
      grid <#> \row ->
        H.div_ "d-flex"
          { style: H.css { lineHeight: 0 } } $
          row <#> \cell ->
            H.div ("d-inline-block m-0 grid-cell-container" <> M.guard (state.play == Just (snd cell)) " active") $
              H.div_ ("d-inline-block grid-cell bg-" <> if Set.member cell state.livingCells then "salmon" else "light")
                { onClick: dispatch <| ToggleCell cell }
                H.empty

    -- TODO: show states change on hover
    presets =
      H.div "row" $ Preset.all <#> Preset.livingCells <#> \cells ->
        H.div "col-6 col-sm-4 col-md-3 pb-3" $
          H.div_ "preset d-flex rounded overflow-hidden border"
            { onClick: dispatch <| LoadPreset cells } $
            H.div "preset-grid mx-auto" $
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

-- TODO: make this configurable
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

-- TODO: make this configurable
beatsPerMeasure :: Int
beatsPerMeasure = 16

-- TODO: move grid logic to its own module
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
