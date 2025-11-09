module Life.Types.Game.RLE.Expr
  ( Expr(..)
  , arrayCodec
  , codec
  )
  where

import Prelude

import Data.Codec as C
import Data.Either (hush)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.String as S
import Data.String.Regex (Regex)
import Data.String.Regex as R
import Data.String.Regex.Flags as RF
import Life.Types.Codec (Codec)
import Life.Types.Codec as Codec
import Partial.Unsafe (unsafePartial)

data Expr
  = Alive Int
  | Dead Int
  | Newline Int
  | End

arrayCodec :: Codec String (Array Expr)
arrayCodec = Codec.match exprRegex codec

codec :: Codec String Expr
codec = C.codec decode encode
  where
    decode s = s # S.splitAt (S.length s - 1) # decodeParts

    decodeParts { before, after }
      | after == "o" = Int.fromString before # fromMaybe 1 # Alive # Just
      | after == "b" = Int.fromString before # fromMaybe 1 # Dead # Just
      | after == "$" = Int.fromString before # fromMaybe 1 # Newline # Just
      | after == "!" = Just End
      | otherwise = Nothing

    encode = case _ of
      Alive n
        | n < 1 -> ""
        | n == 1 -> "o"
        | otherwise -> show n <> "o"
      Dead n
        | n < 1 -> ""
        | n == 1 -> "b"
        | otherwise -> show n <> "b"
      Newline n
        | n < 1 -> ""
        | n == 1 -> "$"
        | otherwise -> show n <> "$"
      End -> "!"

exprRegex :: Regex
exprRegex = unsafePartial fromJust $ hush $ R.regex "[0-9]*[boxyz$]|!" RF.global
