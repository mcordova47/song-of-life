-- | Cominators for creating codecs.
module Life.Types.Codec
  ( (/>)
  , (</)
  , (</>)
  , (<\)
  , (<\>)
  , (\>)
  , Codec
  , chars
  , class Serializable
  , codec
  , discardFirst
  , discardSecond
  , enum
  , int
  , join
  , literal
  , ljoin
  , match
  , rdiscardFirst
  , rdiscardSecond
  , rjoin
  , rurl
  , url
  )
  where

import Prelude hiding (join)

import Control.Alternative (guard)
import Data.Array as Array
import Data.Bounded.Generic (class GenericBottom)
import Data.Codec as C
import Data.Enum.Generic (class GenericBoundedEnum, class GenericEnum)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap)
import Data.String (Pattern(..))
import Data.String as S
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex as R
import Data.Traversable (foldMap, traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Life.Utils.Generic as G

class Serializable a where
  codec :: Codec String a

type Codec a b = C.Codec Maybe a a b b

join :: forall a b. (S.Pattern -> String -> Maybe Int) -> String -> Codec String a -> Codec String b -> Codec String (a /\ b)
join indexOf s a b = C.codec decode encode
  where
    decode str = do
      i <- indexOf (S.Pattern s) str
      let
        sa = S.take i str
        sb = S.drop (i + 1) str
      a' <- C.decode a sa
      b' <- C.decode b sb
      pure (a' /\ b')

    encode (a' /\ b') =
      C.encode a a' <> s <> C.encode b b'

ljoin :: forall a b. String -> Codec String a -> Codec String b -> Codec String (a /\ b)
ljoin = join S.indexOf

rjoin :: forall a b. String -> Codec String a -> Codec String b -> Codec String (a /\ b)
rjoin = join S.lastIndexOf

url :: forall a b. Codec String a -> Codec String b -> Codec String (a /\ b)
url = ljoin "/"

infixr 9 url as </>

rurl :: forall a b. Codec String a -> Codec String b -> Codec String (a /\ b)
rurl = rjoin "/"

infixr 9 rurl as <\>

discardFirst :: forall a. Codec String Unit -> Codec String a -> Codec String a
discardFirst u a = discardFirst' (u </> a)

infixr 9 discardFirst as />

discardSecond :: forall a. Codec String a -> Codec String Unit -> Codec String a
discardSecond a u = discardSecond' (a </> u)

infixr 9 discardSecond as </

rdiscardFirst :: forall a. Codec String Unit -> Codec String a -> Codec String a
rdiscardFirst u a = discardFirst' (u <\> a)

infixr 9 rdiscardFirst as \>

rdiscardSecond :: forall a. Codec String a -> Codec String Unit -> Codec String a
rdiscardSecond a u = discardSecond' (a <\> u)

infixr 9 rdiscardSecond as <\

discardFirst' :: forall a s. Codec s (Unit /\ a) -> Codec s a
discardFirst' = dimap toTuple fromTuple
  where
    toTuple a' = unit /\ a'
    fromTuple (_ /\ a') = a'

discardSecond' :: forall a s. Codec s (a /\ Unit) -> Codec s a
discardSecond' = dimap toTuple fromTuple
  where
    toTuple a' = a' /\ unit
    fromTuple (a' /\ _) = a'

literal :: forall a. Eq a => a -> Codec a Unit
literal a = C.codec (\a' -> guard (a' == a) *> pure unit) (const a)

int :: Codec String Int
int = C.codec Int.fromString show

match :: forall a. Regex -> Codec String a -> Codec String (Array a)
match regex codec = C.codec decode encode
  where
    decode =
      R.match regex
      >=> traverse identity
      >>> map Array.fromFoldable
      >=> traverse (C.decode codec)

    encode =
      foldMap (C.encode codec)

chars :: Codec String (Array String)
chars =
  C.codec (Just <<< String.split (Pattern "")) Array.fold

enum :: forall @a rep
  . Generic a rep
  => GenericBottom rep
  => GenericEnum rep
  => GenericBoundedEnum rep
  => (a -> String)
  -> Codec String a
enum encode = C.codec decode encode
  where
    decode = G.decodeTag encode
