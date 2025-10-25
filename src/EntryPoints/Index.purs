module EntryPoints.Index where

import Prelude

import Effect (Effect)
import Elmish (ComponentDef)
import Elmish.Boot (defaultMain)
import Life.Pages.Main as Main
import Life.Types.Game.Bounded (Bounded)

main :: Effect Unit
main = defaultMain { def, elementId: "app" }

def :: ComponentDef (Main.Message Bounded) (Main.State Bounded)
def =
  { init: Main.init
  , update: Main.update
  , view: Main.view
  }
