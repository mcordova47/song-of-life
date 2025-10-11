module Life.Types.Grid.Instruction
  ( Instruction(..)
  , arrayCodec
  , codec
  )
  where

import Prelude

import Data.Codec as C
import Data.Either (hush)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromJust)
import Data.String as S
import Data.String.Regex (Regex)
import Data.String.Regex as R
import Data.String.Regex.Flags as RF
import Life.Types.Codec (Codec)
import Life.Types.Codec as Codec
import Partial.Unsafe (unsafePartial)

data Instruction
  = Move Int
  | TurnOn Int

instance Show Instruction where
  show = case _ of
    Move n -> "m" <> show n
    TurnOn n -> "o" <> show n

arrayCodec :: Codec String (Array Instruction)
arrayCodec = Codec.match instructionRegex codec

codec :: Codec String Instruction
codec = C.codec decode encode
  where
    decode = S.splitAt 1 >>> decodeParts

    decodeParts { before, after }
      | before == "m" = Move <$> Int.fromString after
      | before == "o" = TurnOn <$> Int.fromString after
      | otherwise = Nothing

    encode = case _ of
      Move n -> "m" <> show n
      TurnOn n -> "o" <> show n

instructionRegex :: Regex
instructionRegex = unsafePartial fromJust $ hush $ R.regex "[mo][0-9]+" RF.global
