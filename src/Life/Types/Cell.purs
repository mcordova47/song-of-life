module Life.Types.Cell
  ( Cell
  , codec
  )
  where

import Data.Argonaut (Json)
import Data.Codec.Argonaut.Common (Codec, JsonDecodeError)
import Data.Codec.Argonaut.Common as C
import Data.Either (Either)
import Data.Tuple.Nested (type (/\))

type Cell = Int /\ Int

codec ∷ Codec (Either JsonDecodeError) Json Json Cell Cell
codec = C.tuple C.int C.int
