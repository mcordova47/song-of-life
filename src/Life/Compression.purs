module Life.Compression
  ( codec
  )
  where

import Prelude

import Data.Argonaut as J
import Data.Codec (Codec)
import Data.Codec as C
import Data.Codec.Argonaut (JsonDecodeError(..))
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn1, runFn1)
import Data.Maybe (maybe)
import Data.Nullable (Nullable)
import Data.Nullable as N

codec ∷ Codec (Either JsonDecodeError) String String String String
codec = C.codec decompress compress

compress :: String -> String
compress =
  runFn1 compress_

decompress :: String -> Either JsonDecodeError String
decompress encoded = encoded
  # runFn1 decompress_
  # N.toMaybe
  # maybe (Left $ UnexpectedValue $ J.fromString encoded) Right

foreign import compress_ :: Fn1 String String

foreign import decompress_ :: Fn1 String (Nullable String)