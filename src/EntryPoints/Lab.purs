module EntryPoints.Lab
  ( main
  )
  where

import Prelude

import Control.Alternative (guard)
import Data.Array ((!!))
import Data.Array as Array
import Data.Array.NonEmpty as NA
import Data.Foldable (foldMap, maximumBy)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord.Down (Down(..))
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
import Elmish (Dispatch, ReactElement, Transition, fork, forkMaybe, forkVoid, (<|))
import Elmish.Boot (defaultMain)
import Elmish.Dispatch as E
import Elmish.HTML.Styled as H
import Elmish.Hooks as Hooks
import Life.Components.GridScene (useGridScene)
import Life.Components.GridScene as GridScene
import Life.Components.Header as Header
import Life.Components.Icons as I
import Life.Components.TagSelect as TagSelect
import Life.Hooks.UseMutableRef (useMutableRef)
import Life.Types.Game.Engines.Optimized.Unbounded (Unbounded)
import Life.Types.Game.Life (class CellularAutomaton, class InteractiveAutomaton)
import Life.Types.Game.Life as Life
import Life.Types.Game.NamedRule (NamedRule)
import Life.Types.Game.NamedRule as NamedRule
import Life.Types.Grid.Cell (Cell)
import Life.Types.Music.Letter (Letter(..))
import Life.Types.Music.Modifier (natural)
import Life.Types.Music.Note (Note)
import Life.Types.Music.Note as N
import Life.Types.Music.PitchClass ((//))
import Life.Types.Music.ScaleType (ScaleType)
import Life.Types.Music.ScaleType as ScaleType
import Life.Types.Music.Voicing (Voicing)
import Life.Types.Music.Voicing as Voicing
import Life.Types.Music.Wave as W
import Life.Utils ((:=))

-- Volume of chord based on density of grid
-- n voices (voicesPlaying :: Int, get n - voicesPlaying notes to play)
-- enable editing
-- presets
-- select wave for voices, chords
-- select key
-- select rule
-- exclude off-grid cells

type State =
  { chord ::
      { stop :: Effect Unit
      , root :: Maybe Note
      }
  , harmonyPlaying :: Boolean
  , melodyPlaying :: Boolean
  , playing :: Boolean
  , rule :: NamedRule
  , scale :: ScaleType
  , step :: Int
  , zoom :: Number
  }

data Message f
  = Pause
  | Play
  | PlayMelody Milliseconds
  | Reset
  | SelectRule NamedRule
  | SetChord { root :: Maybe Note, stop :: Effect Unit }
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
  { chord:
      { stop: pure unit
      , root: Nothing
      }
  , harmonyPlaying: false
  , melodyPlaying: false
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
  SelectRule rule ->
    pure state { rule = rule }
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
    rowCount acc (r /\ _) = Map.alter (fromMaybe 0 >>> (+) 1 >>> Just) r acc
    rowCounts s = s.game # Life.toCells # Array.fromFoldable # Array.foldl rowCount Map.empty # Map.toUnfoldable :: Array (Int /\ Int)
    chordNotes' = chordNotes state.scale
    melodyNotes' = melodyNotes state.scale
    chord row =
      chordNotes'
      <> chordNotes'
      # Array.drop ((row - minRow state) * Array.length chordNotes' / numRows)
      # Array.take 3
    melody row = melodyNotes' !! ((row - minRow state) * Array.length melodyNotes' / numRows)

    playHarmony s prev
      | state.harmonyPlaying = pure Nothing
      | otherwise = playNote harmonyCell s prev

    playMelody s prev
      | state.melodyPlaying = pure Nothing
      | otherwise = playNote melodyCell s prev

    playNote cellSelector s prev = do
      let bornCells = Set.difference (Life.toCells s.game) (Life.toCells prev.game) # Array.fromFoldable
      case cellSelector bornCells of
        Just (row /\ col) -> do
          let
            neighbors = Set.fromFoldable do
              row' <- [row - 1, row, row + 1]
              col' <- [col - 1, col, col + 1]
              guard (row' /= 0 || col' /= 0)
              pure (row' /\ col')
            livingNeighbors = Set.intersection (Life.toCells s.game) neighbors # Set.size
            ms = Int.toNumber livingNeighbors * 62.5
          case melody row of
            Just note | ms > 0.0 -> do
              N.play (Milliseconds ms) W.Triangle note
              pure $ Just $ PlayMelody (Milliseconds ms)
            _ ->
              pure Nothing
        _ ->
          pure Nothing

    playChord bufferRef ms s = do
      buffer <- Ref.read bufferRef
      let
        buffer' = buffer + ms
        row = rowCounts s # maximumBy (comparing snd) <#> fst # fromMaybe 0
        notes' = chord row
        root = Array.head notes'
      if root /= state.chord.root && buffer' >= measureDuration then do
        bufferRef := buffer' - measureDuration
        state.chord.stop
        { stop } <- foldMap (N.drone W.Sawtooth) notes'
        pure $ Just $ SetChord { root, stop }
      else do
        bufferRef := buffer'
        pure Nothing

view :: forall @f. InteractiveAutomaton f => Eq (f Boolean) => State -> Dispatch (Message f) -> ReactElement
view state dispatch = Hooks.component Hooks.do
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
      , H.div "mt-3" $
          H.div_ ""
            { style: H.css { maxWidth: "300px" } } $
            TagSelect.view
              { display: NamedRule.display
              , onChange: dispatch <<< SelectRule
              , value: state.rule
              }
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
      , speed: 35
      , width
      , zoom: state.zoom
      }

melodyCell :: Array (Int /\ Int) -> Maybe (Int /\ Int)
melodyCell bornCells =
  groupedCells bornCells
    # Array.head
    <#> NA.head

harmonyCell :: Array (Int /\ Int) -> Maybe (Int /\ Int)
harmonyCell bornCells =
  groupedCells bornCells
    # flip Array.index 1
    <#> NA.head

groupedCells :: Array (Int /\ Int) -> Array (NA.NonEmptyArray (Int /\ Int))
groupedCells bornCells =
  if Array.null bornCells then []
  else
    let
      groupedByCol =
        bornCells
        # Array.groupBy (\(_ /\ c1) (_ /\ c2) -> c1 == c2)
        # Array.sortBy (comparing $ Down <<< NA.length)
    in case Array.head groupedByCol of
      Nothing -> []
      Just colGroup ->
        colGroup
        # Array.fromFoldable
        # Array.groupBy (\(r1 /\ _) (r2 /\ _) -> r1 == r2)
        # Array.sortBy (comparing $ Down <<< NA.length)

minRow :: State -> Int
minRow { zoom } =
  Int.floor (fst defaultOrigin - Int.toNumber height / zoom / 2.0)

defaultOrigin :: Number /\ Number
defaultOrigin =
  (Int.toNumber height / defaultZoom / 2.0) /\ (Int.toNumber width / defaultZoom / 2.0)

height :: Int
height = 500

width :: Int
width = 500

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

chordNotes :: ScaleType -> Array Note
chordNotes = notes 14 Voicing.spaced

melodyNotes :: ScaleType -> Array Note
melodyNotes = notes 22 Voicing.default

notes :: Int -> Voicing -> ScaleType -> Array Note
notes n voicing scale = ScaleType.notes' voicing 3 { key: C // natural, notes: n, root: 0, scale }

measureDuration :: Number
measureDuration = 2000.0
