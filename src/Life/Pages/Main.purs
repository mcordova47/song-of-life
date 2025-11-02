module Life.Pages.Main
  ( Message
  , State
  , init
  , update
  , view
  )
  where

import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.Codec as C
import Data.Foldable (foldr, for_, intercalate)
import Data.Int as Int
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid as M
import Data.Set as Set
import Data.String as String
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Elmish (Dispatch, ReactElement, Transition, fork, forkMaybe, forkVoid, forks, (<?|), (<|))
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Life.Components.Header as Header
import Life.Components.Icons as I
import Life.Components.InteractiveGrid as InteractiveGrid
import Life.Components.PresetButton as PresetButton
import Life.Components.ShareButton as ShareButton
import Life.Components.TagSelect as TagSelect
import Life.Types.Game.Life (class Life, class CellularAutomaton)
import Life.Types.Game.Life as Life
import Life.Types.Music.Note (Note)
import Life.Types.Music.Note as Note
import Life.Types.Music.PitchClass (PitchClass)
import Life.Types.Music.PitchClass as PitchClass
import Life.Types.Music.ScaleType (ScaleType)
import Life.Types.Music.ScaleType as ScaleType
import Life.Types.Music.Wave (Wave)
import Life.Types.Music.Wave as Wave
import Life.Types.Game.NamedRule (NamedRule)
import Life.Types.Game.NamedRule as NamedRule
import Life.Types.Preset (Preset)
import Life.Types.Preset as Preset
import Life.Types.Route as Route
import Life.Utils (scrollIntoView)
import Life.Utils.Array as A
import Web.HTML (window)
import Web.HTML.Location as Loc
import Web.HTML.Window (location)

data Message f
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
  | SetGame (f Boolean)
  | SetKey PitchClass
  | SetRule NamedRule
  | SetScale ScaleType
  | SetSpeed Int
  | SetWave Wave
  | ShowCopiedFeedback
  | Step
  | ToggleConnectNotes

type State f =
  { beatsPerMeasure :: Int
  , connectNotes :: Boolean
  , game :: f Boolean
  , key :: PitchClass
  , notes :: Int
  , play :: Maybe Int
  , root :: Int
  , rule :: NamedRule
  , scale :: ScaleType
  , shareHash :: Maybe String
  , showCopiedFeedback :: Boolean
  , speed :: Int
  , wave :: Wave
  }

init :: forall f. CellularAutomaton f => Transition (Message f) (State f)
init = do
  fork $ liftEffect $
    window >>= location >>= Loc.hash <#> String.drop 2 <#> Navigate
  let preset = Preset.empty
  pure
    { beatsPerMeasure: Preset.beatsPerMeasure preset
    , connectNotes: false
    , game: Preset.toLife preset
    , key: Preset.key preset
    , notes: Preset.notes preset
    , play: Nothing
    , root: Preset.root preset
    , rule: NamedRule.default
    , scale: Preset.scale preset
    , shareHash: Nothing
    , showCopiedFeedback: false
    , speed: 5
    , wave: Wave.default
    }

update :: forall f. CellularAutomaton f => State f -> Message f -> Transition (Message f) (State f)
update state = case _ of
  -- TODO: Refactor AutoStep logic:
  --  - state.beatsPerMeasure - 1 hack
  AutoStep | Just _ <- state.play -> do
    let game = Life.step state.rule state.game
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
    pure state { play = Just (-1), shareHash = Just $ ShareButton.shareHash state }
  Reset ->
    pure state { game = Life.empty state.notes state.beatsPerMeasure }
  SetGame game ->
    pure state { game = game }
  SetKey key ->
    pure state { key = key }
  SetRule rule ->
    pure state { rule = rule }
  SetScale s ->
    pure state { scale = s }
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
    pure state { game = Life.step state.rule state.game }
  ToggleConnectNotes ->
    pure state { connectNotes = not state.connectNotes }
  LoadPreset p ->
    pure $ loadPreset p
  where
    autoStep game =
      forks \{ dispatch } -> do
        let
          measure' = measure $ Life.toCells game
          durationMs = duration / Int.toNumber state.speed / Int.toNumber (Array.length measure')
        for_ measure' \notes' -> do
          delay $ Milliseconds durationMs
          liftEffect $ dispatch $ Beat notes' $ Milliseconds durationMs
        liftEffect $ dispatch AutoStep

    measure cells =
      A.grid state.notes state.beatsPerMeasure
      # Array.transpose
      # foldr (connectCells cells) []
      <#> Array.filter (\(_ /\ cell) -> Set.member cell cells)
      <#> map (\(n /\ (row /\ _)) -> n /\ row)
      <#> Array.mapMaybe \(n /\ row) -> (ScaleType.notes Preset.defaultOctave state !! row) <#> (/\) n

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
        , game = Preset.toLife p
        , notes = Preset.notes p
        , root = Preset.root p
        , scale = Preset.scale p
        , wave = Preset.wave p
        }

view :: forall f. Life f => State f -> Dispatch (Message f) -> ReactElement
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
            will change and the next measure will play. The
          """
        , H.a_ "" { href: "#rules" } "rules"
        , H.text " are explained below and you can also try "
        , variantLinks
        , H.text " versions of the game."
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
                [ ShareButton.view "btn text-salmon hover:text-salmon-highlight px-0"
                    state
                    { onCopied: dispatch ShowCopiedFeedback
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
            [ H.div "col-6 col-sm-3 pt-2" $
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
            , H.div "col-6 col-sm-3 pt-2" $
                H.label "form-label w-100"
                [ H.div "fw-bold mb-2" "Scale"
                , TagSelect.view
                    { value: state.scale
                    , onChange: dispatch <<< SetScale
                    , display: ScaleType.display
                    }
                ]
            , H.div "col-6 col-sm-3 pt-2" $
                H.label "form-label w-100"
                [ H.div "fw-bold mb-2" "Rule"
                , TagSelect.view
                    { value: state.rule
                    , onChange: dispatch <<< SetRule
                    , display: NamedRule.display
                    }
                ]
            , H.div "col-6 col-sm-3 pt-2" $
                H.div ""
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
          ]
        , H.div "mt-3"
          [ H.h5_ "" { id: "rules" } "Rules"
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
            [ H.text "This instance of the game is "
            , H.strong "text-salmon" $ Life.label@f
            , H.text " — "
            , H.text $ Life.description@f
            , H.text ". Some other grid types are "
            , variantLinks
            , H.text "."
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
      InteractiveGrid.view state
        { onChangeRoot: dispatch <<< ChangeRoot
        , onSetGame: dispatch <<< SetGame
        }

    presets =
      H.div "row" $ Preset.all <#> \(name /\ p) -> Preset.toLife@f p # \life ->
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

    variantLinks = variants <#> variantLink # sentence

    variantLink { label, href } =
      H.a_ "" { href } label

    variants = case Life.label@f of
      "unbounded" -> [boundedLink, toroidalLink]
      "toroidal" -> [boundedLink, unboundedLink]
      _ -> [unboundedLink, toroidalLink]

    boundedLink = { label: "bounded", href: "/" }
    unboundedLink = { label: "unbounded", href: "/unbounded" }
    toroidalLink = { label: "toroidal", href: "/toroidal" }

    sentence xs = case Array.uncons xs of
      Just { tail } | Array.length tail <= 1 -> intercalate (H.text " and ") xs
      Just { head, tail } -> head <> H.text ", " <> sentence tail
      Nothing -> H.empty

duration :: Number
duration = 15_000.0
