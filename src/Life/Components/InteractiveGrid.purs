module Life.Components.InteractiveGrid
  ( Args
  , Events
  , view
  )
  where

import Prelude

import Data.Array ((!!))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Elmish (ReactElement, Dispatch, (<|))
import Elmish.HTML.Styled as H
import Life.Types.Life (class InteractiveAutomaton)
import Life.Types.Life as Life
import Life.Types.Music.Note ((\\))
import Life.Types.Music.Note as Note
import Life.Types.Music.PitchClass (PitchClass)
import Life.Types.Music.ScaleType (ScaleType)
import Life.Types.Music.ScaleType as ScaleType
import Life.Types.Preset as Preset

type Args f r =
  { beatsPerMeasure :: Int
  , game :: f Boolean
  , key :: PitchClass
  , notes :: Int
  , play :: Maybe Int
  , root :: Int
  , scale :: ScaleType
  | r
  }

type Events f =
  { onChangeRoot :: Dispatch Int
  , onSetGame :: Dispatch (f Boolean)
  }

view :: forall f r. InteractiveAutomaton f => Args f r -> Events f -> ReactElement
view args { onChangeRoot, onSetGame } =
  Life.renderInteractive
    { life: args.game
    , rows: args.notes
    , cols: args.beatsPerMeasure
    , renderRow: \{ row, content } -> fold do
        note@(pitchClass \\ _) <- ScaleType.notes Preset.defaultOctave args !! row
        pure $
          H.div_ "d-flex align-items-center"
          { style: H.css { lineHeight: 0 } }
          [ H.div "position-relative text-secondary text-end align-content-center grid-row-label small me-2"
            [ H.div ("grid-row-label-text text-end" <> guard (pitchClass == args.key) " text-salmon") $
                Note.display note
            , guard (row == 0) $
                H.button_ "btn position-absolute"
                  { onClick: onChangeRoot <| (-1)
                  , style: H.css { bottom: "20px", right: "-35%" }
                  }
                  "▲"
            , guard (row == args.notes - 1) $
                H.button_ "btn position-absolute"
                  { onClick: onChangeRoot <| 1
                  , style: H.css { top: "20px", right: "-35%" }
                  }
                  "▼"
            ]
          , content
          ]
    , renderCol: \{ living, col, onClick } ->
        H.div ("d-inline-block m-0 grid-cell-container" <> guard (args.play == Just col) " active") $
          H.div_ ("d-inline-block grid-cell bg-" <> if living then "salmon" else "light")
            { onClick: onSetGame <| onClick }
            H.empty
    }
