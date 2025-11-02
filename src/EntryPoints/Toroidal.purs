module EntryPoints.Toroidal where

import Prelude

import Effect (Effect)
import Elmish (ComponentDef)
import Elmish.Boot (defaultMain)
import Life.Pages.Main as Main
import Life.Types.Game.Engines.Toroidal (Toroidal)

main :: Effect Unit
main = defaultMain { def, elementId: "app" }

def :: ComponentDef (Main.Message Toroidal) (Main.State Toroidal)
def =
  { init: Main.init
  , update: Main.update
  , view: Main.view
  }
