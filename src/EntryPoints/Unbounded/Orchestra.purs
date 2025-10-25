module EntryPoints.Unbounded.Orchestra
  ( main
  )
  where

import Prelude

import Effect (Effect)
import Elmish (ComponentDef)
import Elmish.Boot (defaultMain)
import Life.Pages.Orchestra as Orchestra
import Life.Types.Game.Unbounded (Unbounded)

main :: Effect Unit
main = defaultMain { def, elementId: "app" }

def :: ComponentDef (Orchestra.Message Unbounded) (Orchestra.State Unbounded)
def =
  { init: Orchestra.init
  , update: Orchestra.update
  , view: Orchestra.view
  }
