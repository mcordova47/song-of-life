module Life.Hooks.UseInit
  ( UseInit
  , useInit
  )
  where

import Prelude

import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Tuple (curry)
import Data.Tuple.Nested (type (/\))
import Effect.Aff (Aff)
import Elmish (Dispatch, fork)
import Elmish.Component (ComponentName(..))
import Elmish.Hooks (Hook, HookType, mkHook)

foreign import data UseInit :: Type -> HookType

useInit :: ∀ @s. (Aff s) -> Hook (UseInit s) (s /\ Dispatch s)
useInit init =
  mkHook (ComponentName "UseInit") \render ->
    { init: do
        fork init
        pure Nothing
    , update: const <<< pure
    , view: \s d -> s # foldMap \s' -> curry render s' d
    }
