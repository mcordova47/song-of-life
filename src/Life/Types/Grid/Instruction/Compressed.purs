module Life.Types.Grid.Instruction.Compressed
  ( arrayCodec
  , codec
  , instructionRegex
  )
  where

import Prelude

import Control.Alternative (guard)
import Data.Array ((..))
import Data.Array as A
import Data.Char as Char
import Data.Codec as C
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.String.CodeUnits as CU
import Data.String.Regex (Regex)
import Data.String.Regex as R
import Data.String.Regex.Flags as RF
import Life.Types.Codec (Codec)
import Life.Types.Codec as Codec
import Life.Types.Grid.Instruction (Instruction(..))
import Partial.Unsafe (unsafePartial)

arrayCodec :: Codec String (Array Instruction)
arrayCodec = Codec.match instructionRegex codec

instructionRegex :: Regex
instructionRegex = unsafePartial $ fromJust $ hush $ R.regex "[a-z]+|[A-Z]+" RF.global

codec :: Codec String Instruction
codec = C.codec decode encode
  where
    decode s = do
      let codes = s # CU.toCharArray <#> Char.toCharCode
      guard $ not A.null codes
      let
        isMove = A.all (\e -> e >= moveRange.min && e <= moveRange.max) codes
        isOn = A.all (\e -> e >= onRange.min && e <= onRange.max) codes
      guard $ isMove || isOn
      let
        range = if isMove then moveRange else onRange
        diff = range.min - 1
        allCodes = range.min .. range.max
        digits = codes <#> \c -> c - diff
      n <- fromBase (A.length allCodes) digits
      pure if isMove then Move n else TurnOn n

    encode = toCharCodes
      >>> A.mapMaybe Char.fromCharCode
      >>> CU.fromCharArray

    toCharCodes = case _ of
      Move n -> n # inBase (moveRange.max - moveRange.min + 1) # fromMaybe [] <#> ((+) moveRange.min >>> (_ - 1))
      TurnOn n -> n # inBase (onRange.max - onRange.min + 1) # fromMaybe [] <#> ((+) onRange.min >>> (_ - 1))

    moveRange = { min: 97, max: 122 }
    onRange = { min: 65, max: 90 }

inBase :: Int -> Int -> Maybe (Array Int)
inBase b n
  | n < 0 = Nothing
  | b <= 0 = Nothing
  | otherwise = Just $ go n []
  where
    go m r
      | m < b = [m] <> r
      | otherwise = go (m / b) [mod m b] <> r

fromBase :: Int -> (Array Int) -> Maybe Int
fromBase b digits
  | A.any (_ > b) digits = Nothing
  | otherwise = Just $ A.foldl go 0 digits
  where
    go n d = n * b + d
