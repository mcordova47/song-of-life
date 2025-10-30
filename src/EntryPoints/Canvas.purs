module EntryPoints.Canvas where

import Prelude

import Data.Monoid (guard)
import Effect (Effect)
import Elmish (ReactElement)
import Elmish.Boot (defaultMain)
import Life.Components.Canvas as Canvas

main :: Effect Unit
main = defaultMain
  { def: { init: pure unit, view: const $ const view, update: const absurd }
  , elementId: "app"
  }

view :: ReactElement
view = Canvas.component
  { id: "canvas"
  , className: ""
  , height: 500
  , width: 500
  , init: { showSquare: false }
  , onMouseDown: \_ state -> pure state { showSquare = not state.showSquare }
  , view: \state ->
      guard state.showSquare $
        Canvas.Rect
          { x: 225.0
          , y: 225.0
          , width: 50.0
          , height: 50.0
          , fill: "#999999"
          }
  }
