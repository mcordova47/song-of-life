module Life.Hooks.UseMutableRef
  ( UseMutableRef
  , useMutableRef
  )
  where

import Prelude hiding ((>>=))

import Data.Tuple (fst)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Elmish.Hooks (type (<>), Hook)
import Life.Hooks.UseInit (UseInit, useInit)

type UseMutableRef s t = UseInit (Ref s) <> t

useMutableRef :: forall s. s -> Hook (UseMutableRef s) (Ref s)
useMutableRef init =
  useInit (liftEffect $ Ref.new init) <#> fst
