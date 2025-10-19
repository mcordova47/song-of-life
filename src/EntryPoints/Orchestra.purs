module EntryPoints.Orchestra where

import Prelude

import Data.Array ((!!), (..))
import Data.Array as Array
import Data.Codec as C
import Data.Foldable (foldr, for_)
import Data.Int as Int
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid as M
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Elmish (Dispatch, ReactElement, Transition, forkMaybe, forkVoid, forks, (<?|), (<|))
import Elmish.Boot (defaultMain)
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Life.Components.Header as Header
import Life.Components.PresetButton as PresetButton
import Life.Components.TagSelect as TagSelect
import Life.Game.Bounded as Game
import Life.Icons as I
import Life.Types.Cell (Cell)
import Life.Types.Music.Letter (Letter(..))
import Life.Types.Music.Modifier (flat)
import Life.Types.Music.Note (Note, (\\))
import Life.Types.Music.Note as Note
import Life.Types.Music.PitchClass (PitchClass, (//))
import Life.Types.Music.PitchClass as PitchClass
import Life.Types.Music.Scale as Scale
import Life.Types.Music.ScaleType (ScaleType)
import Life.Types.Music.ScaleType as ScaleType
import Life.Types.Music.Wave as Wave
import Life.Types.Preset (Preset)
import Life.Types.Preset as Preset
import Life.Utils (scrollIntoView)

main :: Effect Unit
main = defaultMain { def: { init, view, update }, elementId: "app" }

data Message
  = AutoStep
  | Beat (Array (Int /\ Note)) Milliseconds
  | ChangeRoot Int
  | GenerateRandom
  | LoadPreset Preset
  | Pause
  | Play
  | Reset
  | SetKey PitchClass
  | SetScale ScaleType
  | SetSpeed Int
  | Step
  | ToggleCell Cell

type State =
  { key :: PitchClass
  , livingCells :: Set Cell
  , play :: Maybe Int
  , root :: Int
  , scale :: ScaleType
  , speed :: Int
  }

init :: Transition Message State
init = pure
  { key: E // flat
  , livingCells: Preset.livingCells Preset.headphones
  , play: Nothing
  , root: 0
  , scale: Preset.scale Preset.default
  , speed: 5
  }

update :: State -> Message -> Transition Message State
update state = case _ of
  -- TODO: Refactor AutoStep logic:
  --  - numCols - 1 hack
  AutoStep | Just _ <- state.play -> do
    let livingCells = step state
    autoStep livingCells
    pure state { livingCells = livingCells, play = Just (numCols - 1) }
  AutoStep ->
    pure state
  Beat notes' (Milliseconds dur) -> do
    forkVoid $ liftEffect $ for_ notes' \(n /\ note) ->
      let
        wave = case Array.findIndex ((==) note) (scale state) of
          Just i
            | i <= 4 -> Wave.Sawtooth
            | i <= 8 -> Wave.Square
            | i <= 12 -> Wave.Triangle
          _ -> Wave.Sine
      in
      Note.play (Milliseconds (dur * Int.toNumber n)) wave note
    pure state { play = inc state.play }
  ChangeRoot degrees ->
    pure state { root = state.root + degrees }
  GenerateRandom -> do
    forkMaybe $ liftEffect $ map LoadPreset <$> Preset.random (numRows state) numCols
    pure state
  LoadPreset p ->
    pure $ loadPreset p
  Pause ->
    pure state { play = Nothing }
  Play -> do
    autoStep state.livingCells
    pure state { play = Just (-1) }
  Reset ->
    pure state { livingCells = Set.empty }
  SetScale s ->
    pure state { scale = s }
  SetKey key ->
    pure state { key = key }
  SetSpeed speed ->
    pure state { speed = speed }
  Step ->
    pure state { livingCells = step state }
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
      Game.transpose (gameGrid state)
      # foldr (connectCells cells) []
      <#> Array.filter (\(_ /\ cell) -> Set.member cell cells)
      <#> map (\(n /\ (row /\ _)) -> n /\ row)
      <#> Array.mapMaybe \(n /\ row) -> (scale state !! row) <#> (/\) n

    connectCells living cells cols = case Array.uncons cols of
      Nothing -> [cells <#> cellDuration]
      Just { head, tail } -> (Array.zipWith smoosh cells head # Array.unzip # \(a /\ b) -> [a] <> [b]) <> tail
      where
        smoosh cella b@(nb /\ cellb)
          | nb > 0 = case cellDuration cella of
              na /\ _
                | na > 0
                , nb > 0 -> ((na + nb) /\ cella) /\ (0 /\ cellb)
                | otherwise -> (na /\ cella) /\ b
          | otherwise = cellDuration cella /\ b

        cellDuration cell
          | Set.member cell living = 1 /\ cell
          | otherwise = 0 /\ cell

    inc = case _ of
      Just n -> Just $ mod (n + 1) numCols
      Nothing -> Nothing

    loadPreset p =
      state
        { key = Preset.key p
        , livingCells = Preset.livingCells p
        , root = Preset.root p
        , scale = Preset.scale p
        }

view :: State -> Dispatch Message -> ReactElement
view state dispatch = H.fragment
  [ Header.view
  , H.div_ "container" { style: H.css { maxWidth: "800px" } } $
      H.div "mt-3 mx-auto"
      [ gridView
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
          ]
        , H.div "mt-3"
          [ H.h5 "" "Presets"
          , presets
          ]
        ]
      ]
  ]
  where
    gridView = H.div ("grid py-4 connected" <> M.guard (isJust state.play) " playing") $
      notes # Array.mapWithIndex \row note@(pitchClass \\ _) ->
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
          , M.guard (row == Array.length notes - 1) $
              H.button_ "btn position-absolute"
                { onClick: dispatch <| ChangeRoot 1
                , style: H.css { top: "20px", right: "-35%" }
                }
                "▼"
          ]
        , H.fragment $
            0 .. (numCols - 1) <#> \col ->
              H.div ("d-inline-block m-0 grid-cell-container" <> M.guard (state.play == Just col) " active") $
                H.div_ ("d-inline-block grid-cell bg-" <> if Set.member (row /\ col) state.livingCells then "salmon" else "light")
                  { onClick: dispatch <| ToggleCell (row /\ col) }
                  H.empty
        ]

    notes = scale state

    presets =
      H.div "row" $ Preset.all <#> \(name /\ p) -> Preset.livingCells p # \cells ->
        H.div_ "col-6 col-sm-4 col-md-3 pb-3 connected"
          { key: name } $
          PresetButton.component
            { name
            , cells
            , grid: gameGrid state
            , onClick: E.handleEffect do
                dispatch $ LoadPreset p
                scrollIntoView Header.id
            }

gameGrid :: State -> Array (Array Cell)
gameGrid s =
  Game.grid (numRows s) numCols

numRows :: State -> Int
numRows state =
  Array.length $ scale state

numCols :: Int
numCols = Preset.beatsPerMeasure

scale :: State -> Array Note
scale state =
  (Scale.shift state.root $ ScaleType.toScale state.scale).notes
    { key: state.key
    , root: Preset.defaultOctave
    , length: Preset.defaultNotes
    }

duration :: Number
duration = 15_000.0

step :: State -> Set Cell
step state =
  Game.step state (numRows state) numCols
