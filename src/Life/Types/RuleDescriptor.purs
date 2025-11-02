module Life.Types.RuleDescriptor
  ( (|/|)
  , B(..)
  , RuleDescriptor(..)
  , S(..)
  , dayAndNight
  , diamoeba
  , display
  , dotLife
  , gnarl
  , highLife
  , life
  , lifeWithoutDeath
  , maze
  , mazeWithMice
  , morley
  , replicator
  , seeds
  , stains
  , walledCities
  )
  where

import Prelude

import Data.Array (foldMap)
import Data.Array as Array
import Data.Codec as C
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (dimap)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Life.Types.Codec (class Serializable, Codec, codec)
import Life.Types.Codec as Codec

data RuleDescriptor = RuleDescriptor B S
derive instance Eq RuleDescriptor

infixl 5 RuleDescriptor as |/|

newtype B = B (Array Int)
derive newtype instance Eq B
derive instance Newtype B _

newtype S = S (Array Int)
derive newtype instance Eq S
derive instance Newtype S _

instance Serializable RuleDescriptor where
  codec = dimap toTuple fromTuple (Codec.ljoin "." codec codec)
    where
      toTuple (b |/| s) = b /\ s
      fromTuple (b /\ s) = b |/| s

instance Serializable B where
  codec = bsCodec "B"

instance Serializable S where
  codec = bsCodec "S"

life :: RuleDescriptor
life = B [3] |/| S [2, 3]

highLife :: RuleDescriptor
highLife = B [3, 6] |/| S [2, 3]

seeds :: RuleDescriptor
seeds = B [2] |/| S []

lifeWithoutDeath :: RuleDescriptor
lifeWithoutDeath = B [3] |/| S [0, 1, 2, 3, 4, 5, 6, 7, 8]

dayAndNight :: RuleDescriptor
dayAndNight = B [3, 6, 7, 8] |/| S [3, 4, 6, 7, 8]

morley :: RuleDescriptor
morley = B [3, 6, 8] |/| S [2, 4, 5]

replicator :: RuleDescriptor
replicator = B [1, 3, 5, 7] |/| S [1, 3, 5, 7]

maze :: RuleDescriptor
maze = B [3] |/| S [1, 2, 3, 4, 5]

mazeWithMice :: RuleDescriptor
mazeWithMice = B [3, 7] |/| S [1, 2, 3, 4, 5]

stains :: RuleDescriptor
stains = B [3, 6, 7, 8] |/| S [2, 3, 5, 6, 7, 8]

dotLife :: RuleDescriptor
dotLife = B [3] |/| S [0, 2, 3]

walledCities :: RuleDescriptor
walledCities = B [4, 5, 6, 7, 8] |/| S [2, 3, 4, 5]

gnarl :: RuleDescriptor
gnarl = B [1] |/| S [1]

diamoeba :: RuleDescriptor
diamoeba = B [3, 5, 6, 7, 8] |/| S [5, 6, 7, 8]

display :: RuleDescriptor -> String
display (RuleDescriptor (B b) (S s)) =
  "B" <> foldMap show b <> "/S" <> foldMap show s

bsCodec :: forall a. Newtype a (Array Int) => String -> Codec String a
bsCodec char = C.codec decode encode
  where
    decode str = case Array.uncons (String.split (Pattern "") str) of
      Just { head, tail } | head == char -> wrap <$> traverse Int.fromString tail
      _ -> Nothing

    encode = unwrap >>> Array.sort >>> Array.nub >>> map show >>> Array.fold >>> ((<>) char)

-- Other rules:

-- | Rule Name        | B/S Code      | Behavior                        | Notes                                            |
-- | ---------------- | ------------- | ------------------------------- | ------------------------------------------------ |
-- | **Anneal**       | B4678/S35678  | Self-stabilizing                | Behaves like crystallization                     |
-- | **Coral**        | B3/S45678     | Growth-based                    | Tends to form coral-like branching structures    |
-- | **Coagulations** | B378/S235678  | Aggregative                     | Forms semi-stable patterns that merge together   |
-- | **Assimilation** | B345/S4567    | Invasive                        | Expands slowly, consuming structures             |
-- | **Move**         | B368/S245     | Like Morley, slightly different | Often confused with Morley but distinct dynamics |
-- | **Serviettes**   | B234/S        | Explosive                       | Every cell eventually dies — dramatic bursts     |

-- | Rule         | B/S      | Behavior                    |
-- | ------------ | -------- | --------------------------- |
-- | **LongLife** | B345/S5  | Oscillatory, long-period    |
-- | **TwoByTwo** | B36/S125 | Tile-like patterns          |
-- | **Flock**    | B3/S12   | Moves like particle systems |

-- | Name                | Rule       | Notes                                                               |
-- | ------------------- | ---------- | ------------------------------------------------------------------- |
-- | **B35/S236**        | `B35/S236` | One of the first discovered self-replicating rules (Langton-style). |
-- | **B25/S4**          | `B25/S4`   | Spontaneous replicators and gliders; highly studied.                |
-- | **Pedestrian Life** | `B38/S23`  | Close to Life, but with new gliders; “pedestrian” movement.         |

-- | Name                  | Rule            | Notes                                                           |
-- | --------------------- | --------------- | --------------------------------------------------------------- |
-- | **Iceballs**          | `B256/S256`     | Generates expanding ice-like bubbles.                           |
-- | **2×2 (Pseudo-Life)** | `B36/S125`      | Behaves like a block cellular automaton; used in Life research. |
