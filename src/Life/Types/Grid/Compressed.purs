module Life.Types.Grid.Compressed
  ( cellsCodec
  , codec
  )
  where

import Prelude

import Data.Array (foldMap)
import Data.Char as Char
import Data.Codec as C
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor (dimap)
import Data.Set (Set)
import Data.String.CodeUnits as CU
import Data.Traversable (fold, traverse)
import Data.Tuple.Nested ((/\))
import Life.Types.Cell (Cell)
import Life.Types.Codec (Codec)
import Life.Types.Grid (Instruction(..), Grid)
import Life.Types.Grid as G

-- TODO: Debug "heart" encoding/decoding
-- TODO: Expose uncompressed as another version
-- TODO: Abstract out cipher codec
codec ∷ Codec String Grid
codec = C.codec decode encode

cellsCodec :: Codec String (Set Cell)
cellsCodec = dimap G.fromCells G.toCells codec

decode ∷ String → Maybe Grid
decode s = do
  parts <- G.decodeGridParts s
  instructions <- decodeInstructions parts.instructions
  pure
    { bounds: parts.bounds
    , instructions
    }
  where
    decodeInstructions str = str
      # CU.toCharArray
      <#> Char.toCharCode
      # traverse fromCharCode

    fromCharCode c
      | c >= 97, c <= 122 = Just $ Move $ c - 96
      | c >= 65, c <= 90 = Just $ TurnOn $ c - 64
      | otherwise = Nothing

encode ∷ Grid -> String
encode { bounds: { start: row /\ col, cols }, instructions } = fold
  [ show cols
  , "c"
  , show row
  , "."
  , show col
  , foldMap encodeInstruction instructions
  ]
  where
    encodeInstruction = case _ of
      Move n | n > 26 -> "z" <> encodeInstruction (Move (n - 26))
      Move n -> Char.fromCharCode (n + 96) # maybe "" CU.singleton
      TurnOn n | n > 26 -> "Z" <> encodeInstruction (TurnOn (n - 26))
      TurnOn n -> Char.fromCharCode (n + 64) # maybe "" CU.singleton
