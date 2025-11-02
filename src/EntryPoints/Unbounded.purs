module EntryPoints.Unbounded
  ( main
  )
  where

import Prelude

import Effect (Effect)
import Elmish (ComponentDef)
import Elmish.Boot (defaultMain)
import Life.Pages.Main as Main
import Life.Types.Game.Engines.Optimized.Unbounded (Unbounded)

main :: Effect Unit
main = defaultMain { def, elementId: "app" }

def :: ComponentDef (Main.Message Unbounded) (Main.State Unbounded)
def =
  { init: Main.init
  , update: Main.update
  , view: Main.view
  }
