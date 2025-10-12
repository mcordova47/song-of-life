module Life.Types.Musc.Modifier
  ( Modifier(..)
  , codec
  , display
  , flat
  , halfSteps
  , natural
  , sharp
  )
  where

import Prelude

import Data.Array (fold, (!!))
import Data.Array as A
import Data.Codec as C
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String as S
import Life.Types.Codec (Codec)

newtype Modifier = Modifier Int
derive instance Eq Modifier
derive newtype instance Semiring Modifier

codec :: Codec String Modifier
codec = C.codec decode encode
  where
    decode s = do
      let parts = S.split (S.Pattern "_") s
      first <- parts !! 0
      let mSecond = parts !! 1 >>= Int.fromString
      case first, mSecond of
        "natural", _ ->
          pure natural
        "flat", Just n -> do
          pure $ Modifier (-n)
        "sharp", Just n -> do
          pure $ Modifier n
        _, _ ->
          Nothing

    encode (Modifier n)
      | n < 0 = "flat_" <> show n
      | n > 0 = "sharp_" <> show n
      | otherwise = "natural"

halfSteps :: Modifier -> Int
halfSteps (Modifier n) = n

natural :: Modifier
natural = Modifier 0

flat :: Modifier
flat = Modifier (-1)

sharp :: Modifier
sharp = Modifier 1

display :: Modifier -> String
display (Modifier n) = fold arr
  where
    arr
      | n >= 0 = A.replicate n "♯"
      | otherwise = A.replicate (-n) "♭"
