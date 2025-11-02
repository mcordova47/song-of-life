module EntryPoints.Orchestra.Toroidal
  ( main
  )
  where

import Prelude

import Effect (Effect)
import Elmish (ComponentDef)
import Elmish.Boot (defaultMain)
import Life.Pages.Orchestra as Orchestra
import Life.Types.Game.Engines.Toroidal (Toroidal)

main :: Effect Unit
main = defaultMain { def, elementId: "app" }

def :: ComponentDef (Orchestra.Message Toroidal) (Orchestra.State Toroidal)
def =
  { init: Orchestra.init
  , update: Orchestra.update
  , view: Orchestra.view
  }
