module EntryPoints.Orchestra.Index
  ( main
  )
  where

import Prelude

import Effect (Effect)
import Elmish (ComponentDef)
import Elmish.Boot (defaultMain)
import Life.Pages.Orchestra as Orchestra
import Life.Types.Game.Engines.Bounded (Bounded)

main :: Effect Unit
main = defaultMain { def, elementId: "app" }

def :: ComponentDef (Orchestra.Message Bounded) (Orchestra.State Bounded)
def =
  { init: Orchestra.init
  , update: Orchestra.update
  , view: Orchestra.view
  }
