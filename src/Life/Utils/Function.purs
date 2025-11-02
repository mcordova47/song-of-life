module Life.Utils.Function where

import Prelude

import Data.Maybe (Maybe, fromMaybe)
import Data.Monoid (power)
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (unwrap)

compose2 :: forall a b c d. (d -> a -> b) -> (d -> b -> c) -> d -> a -> c
compose2 f g d = f d >>> g d

infixr 9 compose2 as >>>>

transaction :: forall a. a -> (a -> Maybe a) -> a
transaction x f = f x # fromMaybe x

times :: forall a. (a -> a) -> Int -> a -> a
times f = unwrap <<< power (Endo f)
