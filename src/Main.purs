module Main where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Int as Int
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid as M
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (fst, snd)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Elmish (Dispatch, ReactElement, Transition, fork, forkMaybe, forkVoid, forks, (<?|), (<|))
import Elmish.Boot (defaultMain)
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Life.Game as Game
import Life.Icons as I
import Life.Music (Note, playNote)
import Life.Types.Cell (Cell)
import Life.Types.Preset (Preset)
import Life.Types.Preset as Preset
import Life.Types.Route as Route
import Life.Types.Wave (Wave)
import Life.Types.Wave as Wave
import Promise as P
import Web.Clipboard as Clipboard
import Web.HTML (window)
import Web.HTML.Location as Loc
import Web.HTML.Window (location, navigator)

main :: Effect Unit
main = defaultMain { def: { init, view, update }, elementId: "app" }

data Message
  = AutoStep
  | Beat (Array Note) Milliseconds
  | GenerateRandom
  | HideCopiedFeedback
  | LoadPreset Preset
  | Navigate String
  | Pause
  | Play
  | Reset
  | SetSpeed Int
  | SetWave Wave
  | ShowCopiedFeedback
  | Step
  | ToggleCell Cell

type State =
  { livingCells :: Set Cell
  , play :: Maybe Int
  , showCopiedFeedback :: Boolean
  , speed :: Int
  , wave :: Wave
  }

init :: Transition Message State
init = do
  fork $ liftEffect $
    window >>= location >>= Loc.hash <#> String.drop 2 <#> Navigate
  pure
    { livingCells: Set.empty
    , play: Nothing
    , showCopiedFeedback: false
    , speed: 5
    , wave: Wave.default
    }

update :: State -> Message -> Transition Message State
update state = case _ of
  -- TODO: Refactor AutoStep logic:
  --  - length - 1 hack
  --  - let Beat drive the engine so that speed and notes can be changed in real time
  AutoStep | Just _ <- state.play -> do
    let livingCells = Game.step state.livingCells
    autoStep livingCells
    pure state { livingCells = livingCells, play = Just (Array.length (measure livingCells) - 1) }
  AutoStep ->
    pure state
  Beat notes' dur -> do
    forkVoid $ liftEffect $ for_ notes' $ playNote dur state.wave
    pure state { play = inc state.play }
  GenerateRandom -> do
    forkMaybe $ liftEffect $ map LoadPreset <$> Preset.random
    pure state
  HideCopiedFeedback ->
    pure state { showCopiedFeedback = false }
  Navigate hash ->
    case Route.decode hash of
      Just (Route.Share preset) ->
        pure state
          { livingCells = Preset.livingCells preset
          , wave = Preset.wave preset
          }
      Nothing ->
        pure state
          { livingCells = Preset.livingCells Preset.default
          , wave = Preset.wave Preset.default
          }
  Pause ->
    pure state { play = Nothing }
  Play -> do
    autoStep state.livingCells
    pure state { play = Just (-1) }
  Reset ->
    pure state { livingCells = Set.empty }
  SetSpeed speed ->
    pure state { speed = speed }
  SetWave wave ->
    pure state { wave = wave }
  ShowCopiedFeedback -> do
    fork do
      delay $ Milliseconds 2000.0
      pure HideCopiedFeedback
    pure state { showCopiedFeedback = true }
  Step ->
    pure state { livingCells = Game.step state.livingCells }
  ToggleCell cell ->
    pure state { livingCells = Set.toggle cell state.livingCells }
  LoadPreset p ->
    pure state
      { livingCells = Preset.livingCells p
      , wave = Preset.wave p
      }
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
      Game.transpose Game.grid
      <#> Array.filter (\cell -> Set.member cell cells)
      <#> map fst
      <#> Array.mapMaybe (Array.index Game.notes)

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
                  { onClick: dispatch <| GenerateRandom
                  , title: "Random"
                  } $
                  I.dice { size: 32 }
            , H.button_ "btn text-salmon hover:text-salmon-highlight p-0 ms-2"
                  { onClick: dispatch <| Reset
                  , title: "Reset"
                  } $
                  I.trash { size: 32 }
            , H.button_ "btn text-salmon hover:text-salmon-highlight p-0 ms-2 me-0"
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
              , H.div "position-relative ms-2"
                [ H.button_ "btn text-salmon hover:text-salmon-highlight px-0"
                    { onClick: E.handleEffect do
                        let hash = Route.encode $ Route.Share $ Preset.fromState state
                        origin <- window >>= location >>= Loc.origin
                        _ <-
                          window >>= navigator >>= Clipboard.clipboard
                            >>= traverse \clipboard ->
                              clipboard
                                # Clipboard.writeText (origin <> "/#/" <> hash)
                                >>= P.then_ \_ -> do
                                  dispatch ShowCopiedFeedback
                                  pure $ P.resolve unit
                        pure unit
                    , title: "Share"
                    } $
                    I.share { size: 32 }
                , M.guard state.showCopiedFeedback $
                    H.div_ "position-absolute d-flex align-items-center ms-2"
                    { style: H.css
                        { top: "50%"
                        , left: "100%"
                        , transform: "translateY(-50%)"
                        }
                    }
                    [ H.div "callout-left callout-secondary translucent" H.empty
                    , H.div "rounded py-2 px-3 bg-secondary text-white translucent text-nowrap" "Copied link to clipboard!"
                    ]
                ]
              ]
        , H.div "mt-3"
          [ H.h5 "" "Controls"
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
          [ H.h5 "" "Presets"
          , presets
          ]
        ]
      ]
  ]
  where
    gridView = H.div ("d-flex flex-column align-items-center mx-auto overflow-auto" <> M.guard (isJust state.play) " playing") $
      Game.grid <#> \row ->
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
            { onClick: dispatch <| LoadPreset $ Preset.fromState { livingCells: cells, wave: state.wave } } $
            H.div "preset-grid mx-auto" $
              Game.grid <#> \row ->
                H.div_ "d-flex"
                  { style: H.css { lineHeight: 0 } } $
                  row <#> \cell ->
                    H.div "d-inline-block m-0 preset-grid-cell-container" $
                      H.div ("d-inline-block preset-grid-cell bg-" <> if Set.member cell cells then "salmon" else "light")
                        H.empty

duration :: Number
duration = 15_000.0
