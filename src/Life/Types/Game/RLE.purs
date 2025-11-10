module Life.Types.Game.RLE where


import Prelude

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Data.Array (elem, foldl, (!!), (..))
import Data.Array as A
import Data.Codec as C
import Data.Foldable (fold, foldMap, maximum, minimum)
import Data.Int as Int
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.String as S
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Life.Types.Codec (class Serializable)
import Life.Types.Game.RLE.Expr (Expr(..))
import Life.Types.Game.RLE.Expr as Expr
import Life.Types.Game.RuleDescriptor (B(..), RuleDescriptor, S(..), (|/|))

-- | A type for .rle (Run Length Encoded) formatted patterns
newtype RLE = RLE
  { author :: Maybe String
  , bounds :: Maybe
      { x :: Int
      , y :: Int
      }
  , comments :: Array String
  , name :: Maybe String
  , pattern :: Array Expr
  , rule :: Maybe RuleDescriptor
  , topLeft :: Maybe (Int /\ Int)
  }
derive instance Newtype RLE _

instance Serializable RLE where
  codec = C.codec decode encode
    where
      decode = S.split (S.Pattern "\n") >>> map S.trim >>> A.filter (not S.null) >>> foldl decodeLine empty >>> Just

      decodeLine (RLE rle) line
        | S.take 2 line == "#O" = RLE rle { author = rle.author <|> Just (line # S.drop 2 # S.trim) }
        | S.take 2 line `elem` ["#C", "#c"] = RLE rle { comments = A.snoc rle.comments (line # S.drop 2 # S.trim) }
        | S.take 2 line == "#N" = RLE rle { name = rle.name <|> Just (line # S.drop 2 # S.trim) }
        | S.take 2 line `elem` ["#P", "#R"] = RLE rle { topLeft = line # S.drop 2 # S.trim # decodeCell }
        | S.take 2 line == "#r" = RLE rle { rule = line # S.drop 2 # S.trim # decodeXLifeRule }
        | S.take 3 line == "x =" ||
          S.take 2 line == "x=" = RLE $ decodeHeader rle line
        | otherwise = RLE rle { pattern = rle.pattern <> (line # C.decode Expr.arrayCodec # fromMaybe []) }

      decodeCell =
        S.split (S.Pattern " ") >>>
        A.filter (not S.null <<< S.trim) >>>
        A.mapMaybe Int.fromString >>> \ints -> do
          guard (A.length ints == 2)
          col <- ints !! 0
          row <- ints !! 1
          pure (row /\ col)

      decodeXLifeRule = S.split (S.Pattern "/") >>> \parts -> do
        guard (A.length parts == 2)
        s <- parts !! 0 <#> S.split (S.Pattern "") >>> A.mapMaybe Int.fromString
        b <- parts !! 1 <#> S.split (S.Pattern "") >>> A.mapMaybe Int.fromString
        pure (B b |/| S s)

      decodeRule = S.split (S.Pattern "/") >>> \parts -> do
        guard (A.length parts == 2)
        first <- parts !! 0 <#> S.split (S.Pattern "")
        bParts <- A.uncons first
        second <- parts !! 1 <#> S.split (S.Pattern "")
        sParts <- A.uncons second
        if bParts.head == "B" && sParts.head == "S" then do
          let
            b = bParts.tail # A.mapMaybe Int.fromString
            s = sParts.tail # A.mapMaybe Int.fromString
          pure (B b |/| S s)
        else do
          let
            s = first # A.mapMaybe Int.fromString
            b = second # A.mapMaybe Int.fromString
          pure (B b |/| S s)

      decodeHeader rle =
        S.replaceAll (S.Pattern " ") (S.Replacement "") >>>
        S.split (S.Pattern ",") >>> \parts ->
          (decodeBounds rle parts) { rule = parts # A.find ((==) "rule=" <<< S.take 5) <#> S.drop 5 >>= decodeRule }

      decodeBounds rle parts = fromMaybe rle do
        xPart <- A.find ((==) "x=" <<< S.take 2) parts
        yPart <- A.find ((==) "y=" <<< S.take 2) parts
        x <- xPart # S.drop 2 # Int.fromString
        y <- yPart # S.drop 2 # Int.fromString
        pure rle { bounds = Just { x, y } }

      encode (RLE rle) =
        [ rle.name <#> ((<>) "#N " >>> A.singleton) # fromMaybe []
        , rle.author <#> ((<>) "#O " >>> A.singleton) # fromMaybe []
        , rle.topLeft <#> (\(row /\ col) -> ["#R " <> show col <> " " <> show row]) # fromMaybe []
        , rle.comments <#> ((<>) "#C ")
        , rle.bounds # maybe [] \{ x, y } -> ["x = " <> show x <> ", y = " <> show y <> (rle.rule # maybe "" \(B b |/| S s) -> ", rule = B" <> foldMap show b <> "/S" <> foldMap show s)]
        , [rle.pattern # C.encode Expr.arrayCodec]
        ]
        # fold
        # A.intercalate "\n"

empty :: RLE
empty = RLE
  { author: Nothing
  , bounds: Nothing
  , comments: []
  , name: Nothing
  , pattern: []
  , rule: Nothing
  , topLeft: Nothing
  }

fromCells :: Set (Int /\ Int) -> RLE
fromCells cells = RLE empty'
  { bounds = Just { x: cols, y: rows }
  , pattern = pattern'
  , topLeft = Just (minRow /\ minCol)
  }
  where
    (RLE empty') = empty

    pattern' = cellArray
      # A.foldl go { pattern: Nil, position: minRow /\ minCol }
      # _.pattern
      # (End : _)
      # A.fromFoldable
      # A.reverse
      where
        go { position: prevRow /\ prevCol, pattern } (row /\ col) =
          { position: row /\ col
          , pattern: case pattern of
              Nil | row == minRow && col == minCol -> Alive 1 : Nil
              Nil -> Alive 1 : Dead (col - prevCol) : Nil
              _ | row - prevRow >= 1, col == minCol -> Alive 1 : Newline (row - prevRow) : pattern
              _ | row - prevRow >= 1 -> Alive 1 : Dead (col - minCol) : Newline (row - prevRow) : pattern
              Dead _ : _ | col - prevCol == 1 -> Alive 1 : pattern
              Dead n : rest -> Alive 1 : Dead (n + col - prevCol - 1) : rest
              Alive n : rest | col - prevCol == 1 -> Alive (n + 1) : rest
              Alive _ : _ -> Alive 1 : Dead (col - prevCol - 1) : pattern
              Newline _ : _ | col == minCol -> Alive 1 : pattern
              Newline _ : _ -> Alive 1 : Dead (col - minCol) : pattern
              End : _ -> pattern
          }

    cellArray = A.fromFoldable cells

    minCol = cellArray <#> snd # minimum # fromMaybe 0
    maxCol = cellArray <#> snd # maximum # fromMaybe 0
    minRow = cellArray <#> fst # minimum # fromMaybe 0
    maxRow = cellArray <#> fst # maximum # fromMaybe 0
    cols = maxCol - minCol + 1
    rows = maxRow - minRow + 1

toCells :: RLE -> Set (Int /\ Int)
toCells (RLE { pattern, topLeft }) = pattern
  # A.foldl go { cells: [], position: minRow /\ minCol }
  # _.cells
  # Set.fromFoldable
  where
    minRow /\ minCol = topLeft # fromMaybe (0 /\ 0)
    go acc = case _ of
      Alive n ->
        acc
          { cells = acc.cells <> (((/\) row) <$> (col .. (n + col - 1)))
          , position = row /\ (col + n)
          }
      Dead n ->
        acc { position = row /\ (col + n) }
      Newline n ->
        acc { position = (row + n) /\ minCol }
      End ->
        acc
      where
        row /\ col = acc.position
