module EntryPoints.Index where

import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.Codec as C
import Data.Foldable (fold, foldr, for_)
import Data.Int as Int
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid as M
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Elmish (Dispatch, ReactElement, Transition, fork, forkMaybe, forkVoid, forks, (<?|), (<|))
import Elmish.Boot (defaultMain)
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Life.Components.Header as Header
import Life.Components.PresetButton as PresetButton
import Life.Components.TagSelect as TagSelect
import Life.Game.Bounded (Bounded)
import Life.Game.Bounded as Game
import Life.Icons as I
import Life.Types.Cell (Cell)
import Life.Types.Grid as Grid
import Life.Types.Grid.Instruction (Instruction(..))
import Life.Types.Life as Life
import Life.Types.Music.Note (Note, (\\))
import Life.Types.Music.Note as Note
import Life.Types.Music.PitchClass (PitchClass)
import Life.Types.Music.PitchClass as PitchClass
import Life.Types.Music.Scale as Scale
import Life.Types.Music.ScaleType (ScaleType)
import Life.Types.Music.ScaleType as ScaleType
import Life.Types.Music.Wave (Wave)
import Life.Types.Music.Wave as Wave
import Life.Types.Preset (Preset)
import Life.Types.Preset as Preset
import Life.Types.Route as Route
import Life.Utils (scrollIntoView)
import Life.Utils as U
import Promise as P
import Record as Record
import Web.Clipboard as Clipboard
import Web.HTML (window)
import Web.HTML.Location as Loc
import Web.HTML.Window (location, navigator)

main :: Effect Unit
main = defaultMain { def: { init, view, update }, elementId: "app" }

data Message
  = AutoStep
  | Beat (Array (Int /\ Note)) Milliseconds
  | ChangeRoot Int
  | GenerateRandom
  | HideCopiedFeedback
  | LoadPreset Preset
  | Navigate String
  | Pause
  | Play
  | Reset
  | SetGame (Bounded Boolean)
  | SetKey PitchClass
  | SetScale ScaleType
  | SetSpeed Int
  | SetWave Wave
  | ShowCopiedFeedback
  | Step
  | ToggleConnectNotes

type State =
  { beatsPerMeasure :: Int
  , connectNotes :: Boolean
  , game :: Bounded Boolean
  , key :: PitchClass
  , notes :: Int
  , play :: Maybe Int
  , root :: Int
  , scale :: ScaleType
  , shareHash :: Maybe String
  , showCopiedFeedback :: Boolean
  , speed :: Int
  , wave :: Wave
  }

init :: Transition Message State
init = do
  fork $ liftEffect $
    window >>= location >>= Loc.hash <#> String.drop 2 <#> Navigate
  let preset = Preset.empty
  pure
    { beatsPerMeasure: Preset.beatsPerMeasure preset
    , connectNotes: false
    , game: gameFromPreset preset
    , key: Preset.key preset
    , notes: Preset.notes preset
    , play: Nothing
    , root: Preset.root preset
    , scale: Preset.scale preset
    , shareHash: Nothing
    , showCopiedFeedback: false
    , speed: 5
    , wave: Wave.default
    }

update :: State -> Message -> Transition Message State
update state = case _ of
  -- TODO: Refactor AutoStep logic:
  --  - state.beatsPerMeasure - 1 hack
  AutoStep | Just _ <- state.play -> do
    let game = Life.step state.game
    autoStep game
    pure state { game = game, play = Just (state.beatsPerMeasure - 1) }
  AutoStep ->
    pure state
  Beat notes' (Milliseconds dur) -> do
    forkVoid $ liftEffect $ for_ notes' \(n /\ note) -> Note.play (Milliseconds (dur * Int.toNumber n)) state.wave note
    pure state { play = inc state.play }
  ChangeRoot degrees ->
    pure state { root = state.root + degrees }
  GenerateRandom -> do
    forkMaybe $ liftEffect $ map LoadPreset <$> Preset.random state.notes state.beatsPerMeasure
    pure state
  HideCopiedFeedback ->
    pure state { showCopiedFeedback = false }
  Navigate hash ->
    case Route.decode hash of
      Just (Route.Share p) ->
        pure $ loadPreset p
      Nothing ->
        pure $ loadPreset Preset.default
  Pause ->
    pure state { play = Nothing, shareHash = Nothing }
  Play -> do
    autoStep state.game
    pure state { play = Just (-1), shareHash = Just $ shareHash state }
  Reset ->
    pure state { game = gameFromPreset $ Preset.fromState $ Record.merge state { livingCells: Set.empty } }
  SetGame game ->
    pure state { game = game }
  SetScale s ->
    pure state { scale = s }
  SetKey key ->
    pure state { key = key }
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
    pure state { game = Life.step state.game }
  ToggleConnectNotes ->
    pure state { connectNotes = not state.connectNotes }
  LoadPreset p ->
    pure $ loadPreset p
  where
    autoStep game =
      forks \{ dispatch } -> do
        let
          measure' = measure $ gameToCells game
          durationMs = duration / Int.toNumber state.speed / Int.toNumber (Array.length measure')
        for_ measure' \notes' -> do
          delay $ Milliseconds durationMs
          liftEffect $ dispatch $ Beat notes' $ Milliseconds durationMs
        liftEffect $ dispatch AutoStep

    measure cells =
      U.grid state.notes state.beatsPerMeasure
      # U.transpose
      # foldr (connectCells cells) []
      <#> Array.filter (\(_ /\ cell) -> Set.member cell cells)
      <#> map (\(n /\ (row /\ _)) -> n /\ row)
      <#> Array.mapMaybe \(n /\ row) -> (scale state !! row) <#> (/\) n

    connectCells living cells cols = case Array.uncons cols of
      Nothing -> [cells <#> cellDuration]
      Just { head, tail } -> (Array.zipWith smoosh cells head # Array.unzip # \(a /\ b) -> [a] <> [b]) <> tail
      where
        smoosh cella b@(nb /\ cellb)
          | nb > 0, state.connectNotes = case cellDuration cella of
              na /\ _
                | na > 0
                , nb > 0 -> ((na + nb) /\ cella) /\ (0 /\ cellb)
                | otherwise -> (na /\ cella) /\ b
          | otherwise = cellDuration cella /\ b

        cellDuration cell
          | Set.member cell living = 1 /\ cell
          | otherwise = 0 /\ cell

    inc = case _ of
      Just n -> Just $ mod (n + 1) state.beatsPerMeasure
      Nothing -> Nothing

    loadPreset p =
      state
        { beatsPerMeasure = Preset.beatsPerMeasure p
        , key = Preset.key p
        , game = gameFromPreset p
        , notes = Preset.notes p
        , root = Preset.root p
        , scale = Preset.scale p
        , wave = Preset.wave p
        }

view :: State -> Dispatch Message -> ReactElement
view state dispatch = H.fragment
  [ Header.view
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
      , H.div ""
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
                        origin <- window >>= location >>= Loc.origin
                        _ <-
                          window >>= navigator >>= Clipboard.clipboard
                            >>= traverse \clipboard ->
                              clipboard
                                # Clipboard.writeText (shareText origin)
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
          , H.div "row mb-3"
            [ H.div "col-6 col-sm-3 col-lg-4 pt-2" $
                H.label "form-label fw-bold w-100"
                [ H.div "mb-2" "Key"
                , H.select_ "form-select"
                    { onChange: dispatch <?| \e ->
                        SetKey <$> C.decode PitchClass.codec (E.selectSelectedValue e)
                    , value: C.encode PitchClass.codec state.key
                    } $
                    PitchClass.all <#> \key ->
                      H.option_ ""
                        { value: C.encode PitchClass.codec key } $
                        PitchClass.display key
                ]
            , H.div "col-6 col-sm-3 col-lg-4 pt-2" $
                H.label "form-label w-100"
                [ H.div "fw-bold mb-2" "Scale"
                , TagSelect.view
                    { value: state.scale
                    , onChange: dispatch <<< SetScale
                    , display: ScaleType.display
                    }
                ]
            , H.div "col pt-2" $
                H.div_ ""
                { style: H.css { maxWidth: 200 }
                }
                [ H.label_ "form-label fw-bold mb-2"
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
          , H.div "fw-bold mb-2" "Adjacent Notes"
          , H.div "mb-3" $
              H.div_ ("d-inline-block text-center card-btn border rounded py-1 px-3 hover:bg-lightblue" <> M.guard state.connectNotes " connected")
              { onClick: dispatch <| ToggleConnectNotes
              }
              [ H.div "d-flex scale-75"
                [ H.div "grid-cell-container" $
                    H.div "grid-cell bg-salmon" H.empty
                , H.div "grid-cell-container" $
                    H.div "grid-cell bg-salmon" H.empty
                ]
              , H.div "" if state.connectNotes then "Connected" else "Disconnected"
              ]
          , H.label "form-label mb-2" "Wave Type"
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
    gridView = H.div ("grid py-4" <> M.guard (isJust state.play) " playing" <> M.guard state.connectNotes " connected") $
      Life.renderInteractive
        { life: state.game
        , rows: state.notes
        , cols: state.beatsPerMeasure
        , renderRow: \{ row, content } -> fold do
            note@(pitchClass \\ _) <- scale state !! row
            pure $
              H.div_ "d-flex align-items-center"
              { style: H.css { lineHeight: 0 } }
              [ H.div "position-relative text-secondary text-end align-content-center grid-row-label small me-2"
                [ H.div ("grid-row-label-text text-end" <> M.guard (pitchClass == state.key) " text-salmon") $
                    Note.display note
                , M.guard (row == 0) $
                    H.button_ "btn position-absolute"
                      { onClick: dispatch <| ChangeRoot (-1)
                      , style: H.css { bottom: "20px", right: "-35%" }
                      }
                      "▲"
                , M.guard (row == state.notes - 1) $
                    H.button_ "btn position-absolute"
                      { onClick: dispatch <| ChangeRoot 1
                      , style: H.css { top: "20px", right: "-35%" }
                      }
                      "▼"
                ]
              , content
              ]
        , renderCol: \{ living, col, onClick } ->
            H.div ("d-inline-block m-0 grid-cell-container" <> M.guard (state.play == Just col) " active") $
              H.div_ ("d-inline-block grid-cell bg-" <> if living then "salmon" else "light")
                { onClick: dispatch <| SetGame <<< onClick }
                H.empty
        }

    presets =
      H.div "row" $ Preset.all <#> \(name /\ p) -> gameFromPreset p # \life ->
        H.div_ "col-6 col-sm-4 col-md-3 pb-3"
          { key: name } $
          PresetButton.component
            { name
            , life
            , rows: state.notes
            , cols: state.beatsPerMeasure
            , onClick: E.handleEffect do
                dispatch $ LoadPreset p
                scrollIntoView Header.id
            }

    shareText origin = fold
      [ "Made with Songs of Life\n\n"
      , emojiGrid
      , "\n"
      , shareUrl origin
      ]
      where
        { bounds, instructions } = Grid.fromCells $ gameToCells $ state.game

        -- TODO: Better method of finding clusters of living cells
        emojiGrid =
          instructions
            # Array.mapWithIndex (/\)
            >>= instructionEmojis
            # U.chunksOf bounds.cols
            <#> (U.fill (min bounds.cols 6) "⬜️" >>> join "")
            # join "\n"

        join delimiter =
          Array.take 6 >>> Array.intercalate delimiter

        instructionEmojis (index /\ instruction) = case instruction of
          Move n | index == 0 -> Array.replicate n "⬜️"
          Move n -> Array.replicate (n - 1) "⬜️"
          TurnOn n -> Array.replicate n "🟪"

    shareUrl origin =
      origin <> "/#/" <> case state.shareHash of
        Just hash -> hash
        Nothing -> shareHash state

shareHash :: State -> String
shareHash state =
  Route.encode $ Route.Share $ Preset.fromState args
  where
    args = Record.merge state { livingCells: gameToCells state.game }

gameFromPreset :: Preset -> Bounded Boolean
gameFromPreset p = Game.fromCells (Preset.notes p) (Preset.beatsPerMeasure p) (Preset.livingCells p)

gameToCells :: Bounded Boolean -> Set Cell
gameToCells = Game.toCells

scale :: forall r. { key :: PitchClass, notes :: Int, root :: Int, scale :: ScaleType | r } -> Array Note
scale state =
  (Scale.shift state.root $ ScaleType.toScale state.scale).notes
    { key: state.key
    , root: Preset.defaultOctave
    , length: state.notes
    }

duration :: Number
duration = 15_000.0
