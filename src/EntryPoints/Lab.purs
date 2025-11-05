module EntryPoints.Lab
  ( main
  )
  where

import Prelude

import Control.Alternative (guard)
import Data.Array ((!!))
import Data.Array as Array
import Data.Foldable (foldMap, maximumBy)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (delay)
import Effect.Class (liftEffect)
import Effect.Random as R
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Elmish (Dispatch, ReactElement, Transition, fork, forkMaybe)
import Elmish.Boot (defaultMain)
import Elmish.HTML.Styled as H
import Elmish.Hooks as Hooks
import Life.Components.GridScene as GridScene
import Life.Components.Header as Header
import Life.Hooks.UseMutableRef (useMutableRef)
import Life.Types.Game.Engines.Optimized.Unbounded (Unbounded)
import Life.Types.Game.Life (class CellularAutomaton, class InteractiveAutomaton)
import Life.Types.Game.Life as Life
import Life.Types.Grid.Cell (Cell)
import Life.Types.Music.Degree as D
import Life.Types.Music.Letter (Letter(..))
import Life.Types.Music.Modifier (natural)
import Life.Types.Music.Note (Note)
import Life.Types.Music.Note as N
import Life.Types.Music.PitchClass ((//))
import Life.Types.Music.Scale as S
import Life.Types.Music.Wave as W
import Life.Utils ((:=))
import Life.Utils.Function ((>>>>))

-- Idea:
-- The y axis could still be notes (but instead of one per row, a band of rows).
-- The band with the most living cells determines a background chord. Newly born
-- cells are picked at random to determine a note, and the length of the note is
-- determined by its living neighbors. Or length of note could be however long
-- the cell stays alive

type State =
  { chord ::
      { stop :: Effect Unit
      , root :: Maybe Note
      }
  , melodyPlaying :: Boolean
  }

data Message f
  = PlayMelody Milliseconds
  | SetChord { root :: Maybe Note, stop :: Effect Unit }
  | StopMelody
  | Tick (Ref Number) Milliseconds { current :: GridScene.State f, previous :: GridScene.State f }

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
  , melodyPlaying: false
  }

update :: forall f. CellularAutomaton f => State -> (Message f) -> Transition (Message f) State
update state = case _ of
  PlayMelody dur -> do
    fork do
      delay dur
      pure StopMelody
    pure state { melodyPlaying = true }
  SetChord chord ->
    pure state { chord = chord }
  StopMelody ->
    pure state { melodyPlaying = false }
  Tick bufferRef (Milliseconds ms) { current: GridScene.State s, previous: GridScene.State previous } -> do
    forkMaybe $ liftEffect $ playChord bufferRef ms s
    forkMaybe $ liftEffect $ playMelody s previous
    pure state
  where
    numRows = Int.ceil (Int.toNumber height / zoom)
    rowCount acc (r /\ _) = Map.alter (fromMaybe 0 >>> (+) 1 >>> Just) r acc
    rowCounts s = s.game # Life.toCells # Array.fromFoldable # Array.foldl rowCount Map.empty # Map.toUnfoldable :: Array (Int /\ Int)
    chordNotes = S.scale
      [ D.tonal
      , D.octave >>>> D.third
      , D.fifth
      , D.octave >>>> D.seventh
      , D.second
      , D.octave >>>> D.fourth
      , D.sixth
      , D.octave
      , D.third
      , D.octave >>>> D.fifth
      , D.seventh
      , D.octave >>>> D.second
      , D.fourth
      , D.octave >>>> D.sixth
      ]
      # \sc -> S.fundamentalNotes sc (C // natural) 3
    melodyNotes = S.scale
      [ D.tonal
      , D.third
      , D.fifth
      , D.seventh
      , D.second
      , D.fourth
      , D.sixth
      , D.octave
      , D.octave >>>> D.third
      , D.octave >>>> D.fifth
      , D.octave >>>> D.seventh
      , D.octave >>>> D.second
      , D.octave >>>> D.fourth
      , D.octave >>>> D.sixth
      ]
      # \sc -> S.fundamentalNotes sc (C // natural) 3
    band row = (row + numRows / 2) * Array.length chordNotes / numRows

    playMelody s prev =
      if not state.melodyPlaying then do
        let bornCells = Set.difference (Life.toCells s.game) (Life.toCells prev.game) # Array.fromFoldable
        index <- R.randomInt 0 (Array.length bornCells - 1)
        let cell = bornCells !! index
        case cell of
          Just (row /\ col) -> do
            let
              neighbors = Set.fromFoldable do
                row' <- [row - 1, row, row + 1]
                col' <- [col - 1, col, col + 1]
                guard (row' /= 0 || col' /= 0)
                pure (row' /\ col')
              livingNeighbors = Set.intersection (Life.toCells s.game) neighbors # Set.size
              ms = Int.toNumber livingNeighbors * 62.5
            case melodyNotes !! band row of
              Just note | ms > 0.0 -> do
                N.play (Milliseconds ms) W.Triangle note
                pure $ Just $ PlayMelody (Milliseconds ms)
              _ ->
                pure Nothing
          _ ->
            pure Nothing
      else
        pure Nothing

    playChord bufferRef ms s = do
      buffer <- Ref.read bufferRef
      let
        minChordDur = 2000.0
        buffer' = buffer + ms
        row = rowCounts s # maximumBy (comparing snd) <#> fst # fromMaybe 0
        notes = chordNotes <> chordNotes # Array.drop (band row) # Array.take 3
        root = Array.head notes
      if root /= state.chord.root && buffer' >= minChordDur then do
        bufferRef := buffer' - 2000.0
        state.chord.stop
        { stop } <- foldMap (N.drone W.Sawtooth) notes
        pure $ Just $ SetChord { root, stop }
      else do
        bufferRef := buffer'
        pure Nothing

view :: forall @f. InteractiveAutomaton f => Eq (f Boolean) => State -> Dispatch (Message f) -> ReactElement
view _ dispatch = Hooks.component Hooks.do
  bufferRef <- useMutableRef 0.0
  Hooks.pure $
    H.div "vh-100 d-flex flex-column"
    [ Header.view
    , H.div_ "flex-grow-1 d-flex justify-content-center align-items-center"
        { style: H.css { backgroundColor: "#0D0208" }
        } $
        GridScene.component
          { playing: true
          , game: Life.fromCells@f 0 0 start
          , height
          , width: 500
          , zoom
          , backgroundColor: "#0D0208"
          , cellColor: "#00FF41"
          , speed: 35
          , onTick: \dur -> dispatch <<< Tick bufferRef dur
          }
    , H.style "" """
        body { padding-bottom: 0 !important; }
      """
    ]

height :: Int
height = 500

zoom :: Number
zoom = 10.0

-- "rabbits"
start :: Set Cell
start = Set.fromFoldable
  [ -1 /\ -1
  , -1 /\ 4
  , 0 /\ -3
  , 0 /\ -2
  , 1 /\ -2
  , 1 /\ -1
  , 1 /\ 1
  , 1 /\ 2
  , 1 /\ 3
  ]
