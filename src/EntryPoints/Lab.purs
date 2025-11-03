module EntryPoints.Lab
  ( main
  )
  where

import Prelude

import Data.Codec as C
import Data.Monoid (guard)
import Data.Set (Set)
import Data.Set as Set
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Elmish (ReactElement, (<?|), (<|))
import Elmish.Boot (defaultMain)
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Elmish.Hooks as Hooks
import Life.Components.GridScene as GridScene
import Life.Components.Header as Header
import Life.Components.TagSelect as TagSelect
import Life.Types.Game.Engines.Optimized.Unbounded (Unbounded)
import Life.Types.Game.Life as Life
import Life.Types.Grid.Cell (Cell)
import Life.Types.Music.Chord as Chord
import Life.Types.Music.Letter (Letter(..))
import Life.Types.Music.Modifier (natural)
import Life.Types.Music.PitchClass ((//))
import Life.Types.Music.PitchClass as PC
import Life.Types.Music.Wave as W

main :: Effect Unit
main = defaultMain
  { def:
      { init: pure unit
      , view: const $ const view
      , update: const absurd
      }
  , elementId: "app"
  }

view :: ReactElement
view = Hooks.component Hooks.do
  chord /\ setChord <- Hooks.useState Chord.Major
  pitchClass /\ setPitchClass <- Hooks.useState (C // natural)
  wave /\ setWave <- Hooks.useState W.Triangle
  Hooks.pure $
    H.div "vh-100 d-flex flex-column"
    [ Header.view
    , H.div_ "flex-grow-1 d-flex justify-content-center align-items-center"
        { style: H.css { backgroundColor: "#0D0208" }
        } $
        GridScene.component
          { playing: true
          , game: Life.fromCells@Unbounded 0 0 start
          , height: 500
          , width: 500
          , defaultZoom: 3.0
          , backgroundColor: "#0D0208"
          , cellColor: "#00FF41"
          }
    , H.div "container pt-3"
      [ H.label "form-label mb-2" "Wave Type"
      , H.div "row" $ W.all <#> \w ->
          H.div "col-6 col-sm-3 col-lg-2" $
            H.div_ ("border rounded card-btn mb-3" <> guard (w == wave) " active")
              { onClick: setWave <| w } $
              H.div "mx-auto text-center"
              [ H.div "" $
                  W.icon { size: 48 } w
              , H.div "" $ W.display w
              ]
      , H.div "row mb-3"
        [ H.div "col-6 col-sm-3 pt-2" $
            H.label "form-label fw-bold w-100"
            [ H.div "mb-2" "Pitch Class"
            , H.select_ "form-select"
                { onChange: setPitchClass <?| \e ->
                    C.decode PC.codec (E.selectSelectedValue e)
                , value: C.encode PC.codec pitchClass
                } $
                PC.all <#> \pc ->
                  H.option_ ""
                    { value: C.encode PC.codec pc } $
                    PC.display pc
            ]
        , H.div "col-6 col-sm-3 pt-2" $
            H.label "form-label fw-bold w-100"
            [ H.div "fw-bold mb-2" "Chord"
            , TagSelect.view
                { value: chord
                , onChange: setChord
                , display: \c -> PC.display pitchClass <> Chord.display c
                }
            ]
        , H.div "col-12 pt-3" $
            H.button_ "btn bg-salmon hover:bright text-white"
              { onClick: E.handleEffect $
                  Chord.play (Milliseconds 1000.0) wave pitchClass 4 chord Chord.defaultVoicing
              }
              "Play chord"
        ]
      ]
    , H.style "" """
        body { padding-bottom: 0 !important; }
      """
    ]

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
