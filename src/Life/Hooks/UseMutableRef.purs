module Life.Hooks.UseMutableRef
  ( UseMutableRef
  , useMutableRef
  )
  where

import Prelude

import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Elmish.Component (ComponentName(..), fork)
import Elmish.Hooks (Hook, HookType, mkHook)

foreign import data UseMutableRef :: Type -> HookType

useMutableRef :: forall s. s -> Hook (UseMutableRef s) (Ref s)
useMutableRef init =
  mkHook (ComponentName "UseMutableRef") \render ->
    { init: do
        fork $ liftEffect $ Ref.new init
        pure Nothing
    , update: const $ pure <<< Just
    , view: const <<< foldMap render
    }
