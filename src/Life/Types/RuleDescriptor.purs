module Life.Types.RuleDescriptor
  ( (|/|)
  , B(..)
  , RuleDescriptor(..)
  , S(..)
  , dayAndNight
  , display
  , highLife
  , life
  , lifeWithoutDeath
  , maze
  , mazeWithMice
  , morley
  , replicator
  , seeds
  )
  where

import Prelude

import Data.Array (foldMap)

data RuleDescriptor = RuleDescriptor B S
derive instance Eq RuleDescriptor

infixl 5 RuleDescriptor as |/|

newtype B = B (Array Int)
derive newtype instance Eq B

newtype S = S (Array Int)
derive newtype instance Eq S

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

display :: RuleDescriptor -> String
display (RuleDescriptor (B b) (S s)) =
  "B" <> foldMap show b <> "/S" <> foldMap show s

-- Other rules:

-- | Rule Name        | B/S Code      | Behavior                        | Notes                                            |
-- | ---------------- | ------------- | ------------------------------- | ------------------------------------------------ |
-- | **Diamoeba**     | B35678/S5678  | Expanding, chaotic              | Produces diamond-like growing blobs              |
-- | **Anneal**       | B4678/S35678  | Self-stabilizing                | Behaves like crystallization                     |
-- | **Coral**        | B3/S45678     | Growth-based                    | Tends to form coral-like branching structures    |
-- | **Coagulations** | B378/S235678  | Aggregative                     | Forms semi-stable patterns that merge together   |
-- | **Assimilation** | B345/S4567    | Invasive                        | Expands slowly, consuming structures             |
-- | **Move**         | B368/S245     | Like Morley, slightly different | Often confused with Morley but distinct dynamics |
-- | **Stains**       | B3678/S235678 | Blotchy                         | Leads to fuzzy blotches and patterns             |
-- | **Serviettes**   | B234/S        | Explosive                       | Every cell eventually dies — dramatic bursts     |

-- | Rule         | B/S      | Behavior                    |
-- | ------------ | -------- | --------------------------- |
-- | **LongLife** | B345/S5  | Oscillatory, long-period    |
-- | **TwoByTwo** | B36/S125 | Tile-like patterns          |
-- | **DotLife**  | B3/S023  | Creates dotted fractals     |
-- | **Flock**    | B3/S12   | Moves like particle systems |

-- | Name             | Rule           | Notes                                                                  |
-- | ---------------- | -------------- | ---------------------------------------------------------------------- |
-- | **Gnarl**        | `B1/S1`        | Every live cell with one neighbor survives or births; spiraling chaos. |

-- | Name                     | Rule           | Notes                                                               |
-- | ------------------------ | -------------- | ------------------------------------------------------------------- |
-- | **WalledCities**         | `B45678/S2345` | Creates walls and chambers; “city” morphology.                      |

-- | Name                | Rule       | Notes                                                               |
-- | ------------------- | ---------- | ------------------------------------------------------------------- |
-- | **B35/S236**        | `B35/S236` | One of the first discovered self-replicating rules (Langton-style). |
-- | **B25/S4**          | `B25/S4`   | Spontaneous replicators and gliders; highly studied.                |
-- | **Pedestrian Life** | `B38/S23`  | Close to Life, but with new gliders; “pedestrian” movement.         |

-- | Name                  | Rule            | Notes                                                           |
-- | --------------------- | --------------- | --------------------------------------------------------------- |
-- | **Iceballs**          | `B256/S256`     | Generates expanding ice-like bubbles.                           |
-- | **Stains**            | `B3678/S235678` | Forms patchy, stained-glass-like textures.                      |
-- | **2×2 (Pseudo-Life)** | `B36/S125`      | Behaves like a block cellular automaton; used in Life research. |
