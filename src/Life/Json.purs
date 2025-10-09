module Life.Json
  ( codec
  )
  where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut as A
import Data.Argonaut as J
import Data.Codec (Codec)
import Data.Codec as C
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))

codec ∷ Codec (Either CA.JsonDecodeError) String String Json Json
codec = C.codec (J.parseJson >>> convertError) J.stringify

convertError ∷ forall a. Either A.JsonDecodeError a → Either CA.JsonDecodeError a
convertError = case _ of
  Right x -> Right x
  Left e -> Left $ convertError' e
  where
    convertError' = case _ of
      A.TypeMismatch s -> CA.TypeMismatch s
      A.UnexpectedValue v -> CA.UnexpectedValue v
      A.AtIndex i e -> CA.AtIndex i $ convertError' e
      A.AtKey s e -> CA.AtKey s $ convertError' e
      A.Named s e -> CA.Named s $ convertError' e
      A.MissingValue -> CA.MissingValue
