module Life.Types.Music.Duration
  ( Duration(..)
  , toNumber
  )
  where

import Prelude

import Data.Int as Int

data Duration = Duration Int Int
derive instance Eq Duration

toNumber :: Duration -> Number
toNumber (Duration numerator denominator) =
  Int.toNumber numerator / Int.toNumber denominator
