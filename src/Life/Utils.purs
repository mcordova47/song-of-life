module Life.Utils
  ( (:=)
  , padLeft
  , scrollIntoView
  , truthy
  , writeRefFlipped
  )
  where

import Prelude

import Data.Function.Uncurried (Fn1, runFn1)
import Data.String as String
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn1, runEffectFn1)

scrollIntoView :: String -> Effect Unit
scrollIntoView = runEffectFn1 scrollIntoView_

truthy :: forall a. a -> Boolean
truthy = runFn1 truthy_

padLeft :: Int -> String -> String -> String
padLeft n s str
  | n - String.length str > 0 = padLeft (n - 1) s (s <> str)
  | otherwise = str

writeRefFlipped :: forall a. Ref a -> a -> Effect Unit
writeRefFlipped = flip Ref.write

infixr 0 writeRefFlipped as :=

foreign import scrollIntoView_ :: EffectFn1 String Unit

foreign import truthy_ :: forall a. Fn1 a Boolean
