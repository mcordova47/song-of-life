module EntryPoints.Lab
  ( main
  )
  where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Array.NonEmpty as NA
import Data.Codec as C
import Data.Foldable (foldMap, maximumBy)
import Data.Function (on)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number as Number
import Data.Ord.Down (Down(..))
import Data.Semigroup.Foldable (minimum)
import Data.Set (Set)
import Data.Set as Set
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (delay)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Elmish (Dispatch, ReactElement, Transition, fork, forkMaybe, forkVoid, (<?|), (<|))
import Elmish.Boot (defaultMain)
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Elmish.Hooks as Hooks
import Life.Components.GridScene (useGridScene)
import Life.Components.GridScene as GridScene
import Life.Components.Header as Header
import Life.Components.Icons as I
import Life.Components.Select as Select
import Life.Components.TagSelect as TagSelect
import Life.Hooks.UseMutableRef (useMutableRef)
import Life.Types.Codec (codec)
import Life.Types.Game.Engines.Optimized.Unbounded (Unbounded)
import Life.Types.Game.Life (class CellularAutomaton, class InteractiveAutomaton)
import Life.Types.Game.Life as Life
import Life.Types.Game.NamedRule (NamedRule)
import Life.Types.Game.NamedRule as NamedRule
import Life.Types.Grid.Cell (Cell)
import Life.Types.Music.Letter (Letter(..))
import Life.Types.Music.Modifier (natural)
import Life.Types.Music.NamedDuration (NamedDuration)
import Life.Types.Music.NamedDuration as ND
import Life.Types.Music.Note (Note)
import Life.Types.Music.Note as N
import Life.Types.Music.PitchClass (PitchClass, (//))
import Life.Types.Music.PitchClass as PitchClass
import Life.Types.Music.ScaleType (ScaleType)
import Life.Types.Music.ScaleType as ScaleType
import Life.Types.Music.Voicing (Voicing)
import Life.Types.Music.Voicing as Voicing
import Life.Types.Music.Wave as W
import Life.Utils ((:=))
import Life.Utils.Math as M

-- Volume of chord based on density of grid
-- n voices (voicesPlaying :: Int, get n - voicesPlaying notes to play)
-- presets
-- select wave for voices, chords
-- exclude off-grid cells
-- look at origin instead of defaultOrigin

type State =
  { bpm :: Number
  , chord :: Chord
  , harmonyPlaying :: Boolean
  , key :: PitchClass
  , melodyPlaying :: Boolean
  , noteDuration :: NamedDuration
  , playing :: Boolean
  , rule :: NamedRule
  , scale :: ScaleType
  , step :: Int
  , zoom :: Number
  }

type Chord =
  { stop :: Effect Unit
  , root :: Maybe Note
  }

data Message f
  = Pause
  | Play
  | PlayMelody Milliseconds
  | Reset
  | SelectKey PitchClass
  | SelectNoteDuration NamedDuration
  | SelectRule NamedRule
  | SelectScale ScaleType
  | SetBpm Number
  | SetChord Chord
  | Step
  | StopMelody
  | Tick (Ref Number) Milliseconds { current :: GridScene.State f, previous :: GridScene.State f }
  | Zoom Number

main :: Effect Unit
main = defaultMain
  { def:
      { init
      , view: view @Unbounded
      , update
      }
  , elementId: "app"
  }

init :: forall f. Transition (Message f) State
init = pure
  { bpm: 160.0
  , chord:
      { stop: pure unit
      , root: Nothing
      }
  , harmonyPlaying: false
  , key: C // natural
  , melodyPlaying: false
  , noteDuration: ND.Eighth
  , playing: false
  , rule: NamedRule.default
  , scale: ScaleType.Pentatonic
  , step: 0
  , zoom: defaultZoom
  }

update :: forall f. CellularAutomaton f => State -> (Message f) -> Transition (Message f) State
update state = case _ of
  Pause -> do
    forkVoid $ liftEffect state.chord.stop
    pure state { playing = false }
  Play ->
    pure state { playing = true }
  PlayMelody dur -> do
    fork do
      delay dur
      pure StopMelody
    pure state { melodyPlaying = true }
  Reset ->
    pure state { step = 0 }
  SelectKey key ->
    pure state { key = key }
  SelectNoteDuration dur ->
    pure state { noteDuration = dur }
  SelectRule rule ->
    pure state { rule = rule }
  SelectScale scale ->
    pure state { scale = scale }
  SetBpm bpm ->
    pure state { bpm = bpm }
  SetChord c ->
    pure state { chord = c }
  Step ->
    pure state { step = state.step + 1 }
  StopMelody ->
    pure state { melodyPlaying = false }
  Tick bufferRef (Milliseconds ms) { current: GridScene.State s, previous: GridScene.State previous } -> do
    forkMaybe $ liftEffect $ playChord bufferRef ms s
    forkMaybe $ liftEffect $ playMelody s previous
    forkMaybe $ liftEffect $ playHarmony s previous
    pure state
  Zoom zoom | not state.playing ->
    pure state { zoom = zoom }
  Zoom _ ->
    pure state
  where
    numRows = Int.ceil (Int.toNumber height / state.zoom)
    numCols = Int.ceil (Int.toNumber width / state.zoom)
    rowCount acc (r /\ _) = Map.alter (fromMaybe 0 >>> (+) 1 >>> Just) r acc
    rowCounts s =
      s.game
      # Life.toCells
      # Array.fromFoldable
      # Array.filter visible
      # Array.foldl rowCount Map.empty
      # Map.toUnfoldable :: Array _
    distanceFromEdge (row /\ col) = minimum $
      NA.cons'
        (row - minRow state)
        [ minRow state + numRows - row
        , col - minCol state
        , minCol state + numCols - col
        ]
    chordNotes' = chordNotes state
    chord row =
      chordNotes'
      <> chordNotes'
      # Array.drop ((row - minRow state) * Array.length chordNotes' / numRows)
      # Array.take 3
    melodyNotes' = melodyNotes state
    melodyNote = distanceFromEdge >>> (_ `mod` Array.length melodyNotes') >>> Array.index melodyNotes'

    playHarmony s prev
      | state.harmonyPlaying = pure Nothing
      | otherwise = playNote harmonyCell s prev

    playMelody s prev
      | state.melodyPlaying = pure Nothing
      | otherwise = playNote melodyCell s prev

    playNote cellSelector s prev = do
      let bornCells = Set.difference (Life.toCells s.game) (Life.toCells prev.game) # Array.fromFoldable # Array.filter visible
      cellSelector state bornCells >>= (\cell -> melodyNote cell <#> \note -> { cell, note }) # maybe (pure Nothing) \{ cell: row /\ col, note } -> do
        let
          neighbors = Set.fromFoldable do
            row' <- [row - 1, row, row + 1]
            col' <- [col - 1, col, col + 1]
            guard (row' /= 0 || col' /= 0)
            pure (row' /\ col')
          livingNeighbors = Set.intersection (Life.toCells s.game) neighbors # Set.size
          ms = Int.toNumber livingNeighbors * 62.5
        if ms > 0.0 then do
          N.play (Milliseconds ms) W.Triangle note
          pure $ Just $ PlayMelody (Milliseconds ms)
        else
          pure Nothing

    playChord bufferRef ms s = do
      buffer <- Ref.read bufferRef
      let
        buffer' = buffer + ms
        row = rowCounts s # maximumBy (comparing snd) <#> fst # fromMaybe 0
        notes' = chord row
        root = Array.head notes'
        measureDuration = 4.0 * 60_000.0 / state.bpm
      if root /= state.chord.root && buffer' >= measureDuration then do
        bufferRef := buffer' - measureDuration
        state.chord.stop
        { stop } <- foldMap (N.drone W.Sawtooth) notes'
        pure $ Just $ SetChord { root, stop }
      else do
        bufferRef := buffer'
        pure Nothing

    visible (row /\ col) =
      (row >= minRow state && row <= (minRow state + numRows)) &&
      (col >= minCol state && col <= (minCol state + numCols))

view :: forall @f. InteractiveAutomaton f => Eq (f Boolean) => State -> Dispatch (Message f) -> ReactElement
view state dispatch = Hooks.component Hooks.do
  let measureDuration = 4.0 * 60_000.0 / state.bpm
  bufferRef <- useMutableRef measureDuration
  scene /\ setScene <- useGridScene $ sceneArgs bufferRef
  Hooks.pure $
    H.fragment
    [ Header.view
    , H.div_ "container" { style: H.css { maxWidth: "800px" } }
      [ H.div "d-flex justify-content-center align-items-center mt-4" scene
      , H.div "d-flex mt-4" $
          H.div "d-inline-flex align-items-center mx-auto bg-lightblue rounded-pill py-1 px-4"
          [ H.button_ "btn text-salmon hover:text-salmon-highlight p-0 ms-2"
              { onClick: E.handleEffect do
                  dispatch Reset
                  setScene _
                    { game = Life.fromCells (height / Int.floor state.zoom) (width / Int.floor state.zoom) Set.empty }
              , title: "Reset"
              } $
              I.trash { size: 32 }
          , H.button_ "btn text-salmon hover:text-salmon-highlight p-0 ms-2 me-0"
              { onClick: dispatch <| if state.playing then Pause else Play
              , title: if state.playing then "Pause" else "Play"
              }
              if state.playing then
                I.pause { size: 64 }
              else
                I.play { size: 64 }
          , H.button_ "btn text-salmon hover:text-salmon-highlight p-0"
              { onClick: E.handleEffect do
                  dispatch Step
                  setScene \s -> s { game = Life.step state.rule s.game }
              , title: "Step"
              } $
              I.arrowBarRight { size: 32 }
          ]
      , H.div "row"
        [ H.div "col-4 col-md-2 pt-3" $
            Select.view
              { decode: C.decode codec
              , display: PitchClass.display
              , encode: C.encode codec
              , onChange: dispatch <<< SelectKey
              , options: PitchClass.all
              , value: state.key
              }
        , H.div "col-8 col-md-4 pt-3" $
            TagSelect.view
              { display: ScaleType.display
              , onChange: dispatch <<< SelectScale
              , value: state.scale
              }
        , H.div "col-6 col-md-3 pt-3" $
            TagSelect.view
              { display: ND.display
              , onChange: dispatch <<< SelectNoteDuration
              , value: state.noteDuration
              }
        , H.div "col-6 col-md-3 pt-3" $
            H.input_ "form-control"
              { onChange: dispatch <<< SetBpm <?| Number.fromString <<< E.inputText
              , type: "number"
              , value: show state.bpm
              }
        , H.div "col col-md-6 pt-3" $
            TagSelect.view
              { display: NamedRule.display
              , onChange: dispatch <<< SelectRule
              , value: state.rule
              }
        ]
      ]
    , H.style "" """
        body { padding-bottom: 0 !important; }
      """
    ]
  where
    sceneArgs bufferRef =
      { defaultOrigin
      , game: Life.fromCells@f (height / Int.floor state.zoom) (width / Int.floor state.zoom) start
      , height
      , onTick: \dur -> dispatch <<< Tick bufferRef dur
      , onZoom: dispatch <<< Zoom
      , playing: state.playing
      , rule: state.rule
      , stepsPerSecond: 1.0 / (state.noteDuration # ND.toSeconds state.bpm)
      , width
      , zoom: state.zoom
      }

melodyCell :: State -> Array Cell -> Maybe Cell
melodyCell state bornCells =
  groupedCells state bornCells
    # Array.head
    <#> M.geometricMedian

harmonyCell :: State -> Array Cell -> Maybe Cell
harmonyCell state bornCells =
  groupedCells state bornCells
    # flip Array.index 1
    <#> M.geometricMedian

groupedCells :: State -> Array Cell -> Array (NA.NonEmptyArray Cell)
groupedCells state =
  Array.groupBy ((==) `on` tile) >>>
  Array.sortWith (Down <<< NA.length)
  where
    numRows = Int.ceil (Int.toNumber height / state.zoom)
    numCols = Int.ceil (Int.toNumber width / state.zoom)
    tr /\ tc = tiles
    tile (row /\ col) = ((row - minRow state) * tr / numRows) * tc + (col - minCol state) * tc / numCols

minRow :: State -> Int
minRow { zoom } =
  Int.floor (fst defaultOrigin - Int.toNumber height / zoom / 2.0)

minCol :: State -> Int
minCol { zoom } =
  Int.floor (fst defaultOrigin - Int.toNumber width / zoom / 2.0)

defaultOrigin :: Number /\ Number
defaultOrigin =
  (Int.toNumber height / defaultZoom / 2.0) /\ (Int.toNumber width / defaultZoom / 2.0)

height :: Int
height = 600

width :: Int
width = 600

defaultZoom :: Number
defaultZoom = 10.0

-- "rabbits"
start :: Set Cell
start =
  [ 0 /\ -1
  , 0 /\ 4
  , 1 /\ -3
  , 1 /\ -2
  , 2 /\ -2
  , 2 /\ -1
  , 2 /\ 1
  , 2 /\ 2
  , 2 /\ 3
  ]
  <#> adjustOrigin
  # Set.fromFoldable
  where
    originRow /\ originCol = defaultOrigin
    adjustOrigin (row /\ col) =
      (row + Int.floor originRow) /\ (col + Int.floor originCol)

chordNotes :: State -> Array Note
chordNotes = notes 14 Voicing.spaced

melodyNotes :: State -> Array Note
melodyNotes state = notes 20 Voicing.default state

notes :: Int -> Voicing -> State -> Array Note
notes n voicing { key, scale } = ScaleType.notes' voicing 2 { key, notes: n, root: 0, scale }

tiles :: Int /\ Int
tiles = 8 /\ 8
