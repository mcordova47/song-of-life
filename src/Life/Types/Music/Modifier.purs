module Life.Types.Music.Modifier
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

import Data.Array (fold)
import Data.Array as A
import Data.Codec as C
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String as S
import Life.Types.Codec (Codec)

-- | Wraps the number of half steps to modify a given letter by to form a pitch
-- | class. Sharp is +1, flat is -1, and they can stack.
newtype Modifier = Modifier Int
derive instance Eq Modifier
derive newtype instance Semiring Modifier

codec :: Codec String Modifier
codec = C.codec decode encode
  where
    decode s =
      let { before, after } = S.splitAt 1 s
      in
      case before, Int.fromString after of
        "n", _ ->
          pure natural
        "f", Just n ->
          pure $ Modifier (-n)
        "f", _ ->
          pure $ Modifier (-1)
        "s", Just n ->
          pure $ Modifier n
        "s", _ ->
          pure $ Modifier 1
        _, _ ->
          Nothing

    encode (Modifier n)
      | n == -1 = "f"
      | n == 1 = "s"
      | n < 0 = "f" <> show n
      | n > 0 = "s" <> show n
      | otherwise = "n"

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
