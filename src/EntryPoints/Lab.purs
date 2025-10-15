module EntryPoints.Lab where

import Prelude

import Effect (Effect)
import Elmish (Dispatch, ReactElement)
import Elmish.Boot (defaultMain)
import Elmish.HTML.Styled as H
import Life.Components.Header as Header

type State = Unit

type Message = Void

main :: Effect Unit
main = defaultMain
  { def:
      { init: pure unit
      , view
      , update: const absurd
      }
  , elementId: "app"
  }

view :: State -> Dispatch Message -> ReactElement
view _ _ = H.fragment
  [ Header.view
  , H.div_ "container" { style: H.css { maxWidth: "800px" } } $
      H.h2 "text-salmon mt-3" "Lab"
  ]
