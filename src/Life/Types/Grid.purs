module Life.Types.Grid
  ( Bounds
  , Grid
  , Instruction
  , codec
  , decode
  , encode
  , fromCells
  , toCells
  )
  where

import Prelude

import Data.Argonaut as J
import Data.Array (foldMap, (..))
import Data.Array as Array
import Data.Char as Char
import Data.Codec (Codec)
import Data.Codec as C
import Data.Codec.Argonaut (JsonDecodeError(..))
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn1, runFn1)
import Data.Int as Int
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (Nullable)
import Data.Nullable as N
import Data.Set (Set)
import Data.Set as Set
import Data.String.CodeUnits as CU
import Data.Traversable (fold, maximum, minimum, traverse)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Life.Types.Cell (Cell)

type Grid = 
  { bounds :: Bounds
  , instructions :: Array Instruction
  }

type Bounds =
  { start :: Cell
  , cols :: Int
  }

data Instruction
  = Move Int
  | TurnOn Int
instance Show Instruction where
  show = case _ of
    Move n -> "m" <> show n
    TurnOn n -> "o" <> show n

-- TODO: Debug "heart" encoding/decoding
-- TODO: Expose uncompressed as another version
-- TODO: Properly use codecs
codec ∷ Codec (Either JsonDecodeError) String String Grid Grid
codec = C.codec decode encode

decode ∷ String → Either JsonDecodeError Grid
decode s = gridParts # maybe (Left $ UnexpectedValue $ J.fromString s) Right
  where
    gridParts = do
      parts <- runFn1 decodeGridParts_ s # N.toMaybe
      cols <- Int.fromString parts.cols
      row <- Int.fromString parts.row
      col <- Int.fromString parts.col
      instructions <- decodeCompressed parts.instructions
      pure
        { bounds: { start: row /\ col, cols }
        , instructions
        }

    -- decodeInstructions str = str
    --   # R.match instructionRegex
    --   >>= traverse identity
    --   <#> Array.fromFoldable
    --   >>= traverse (String.splitAt 1 >>> decodeCompressed)

    -- decodeInstruction { before, after }
    --   | before == "m" = Move <$> Int.fromString after
    --   | before == "o" = TurnOn <$> Int.fromString after
    --   | otherwise = Nothing

    decodeCompressed str = str
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
  , foldMap encodeCompressed instructions
  ]
  where
    -- encodeInstruction = case _ of
    --   Move n -> "m" <> show n
    --   TurnOn n -> "o" <> show n

    encodeCompressed = case _ of
      Move n | n > 26 -> "z" <> encodeCompressed (Move (n - 26))
      Move n -> Char.fromCharCode (n + 96) # maybe "" CU.singleton
      TurnOn n | n > 26 -> "Z" <> encodeCompressed (TurnOn (n - 26))
      TurnOn n -> Char.fromCharCode (n + 64) # maybe "" CU.singleton

-- instructionRegex ∷ Regex
-- instructionRegex = unsafePartial fromJust $ hush $ R.regex "[mo][0-9]+" RF.global

fromCells :: Set Cell -> Grid
fromCells cells = { bounds, instructions }
  where
    bounds =
      { start: minRow /\ minCol
      , cols
      }

    instructions = indices
      # Array.foldl go { instructions: Nil, position: 0 }
      # _.instructions
      # Array.fromFoldable
      # Array.reverse
      where
        go acc i =
          { position: i
          , instructions: case acc.instructions of
              Nil | i == 0 -> TurnOn 1 : Nil
              Nil -> TurnOn 1 : Move i : Nil
              Move _ : _ | i == acc.position + 1 -> TurnOn 1 : acc.instructions
              Move n : rest -> TurnOn 1 : Move (n + i - acc.position) : rest
              TurnOn n : rest | i == acc.position + 1 -> TurnOn (n + 1) : rest
              TurnOn _ : _ -> TurnOn 1 : Move (i - acc.position) : acc.instructions
          }

    cellArray = Array.fromFoldable cells

    minCol = cellArray <#> snd # minimum # fromMaybe 0
    maxCol = cellArray <#> snd # maximum # fromMaybe 0
    minRow = cellArray <#> fst # minimum # fromMaybe 0
    cols = maxCol - minCol + 1

    indices = cellArray <#> \(row /\ col) -> row * cols + col

toCells :: Grid -> Set Cell
toCells { bounds, instructions } = indices <#> indexToCell # Set.fromFoldable
  where
    indexToCell i
      | i <= 0 = bounds.start
      | otherwise = (fst bounds.start + i / bounds.cols) /\ (snd bounds.start + mod i bounds.cols)

    indices = instructions
      # Array.foldl go { indices: [], position: 0 }
      # _.indices
      where
        go acc = case _ of
          Move n ->
            acc { position = acc.position + n }
          TurnOn n ->
            acc
              { indices = acc.indices <> (acc.position .. (n + acc.position - 1))
              , position = acc.position + n - 1
              }

foreign import decodeGridParts_ :: Fn1 String (Nullable { cols :: String, row :: String, col :: String, instructions :: String})
