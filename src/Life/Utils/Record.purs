module Life.Utils.Record
  ( (><)
  , trim
  )
  where

import Prim.Row (class Union)
import Record as Record
import Unsafe.Coerce (unsafeCoerce)

trim :: forall @b @a r. Union b r a => Record a -> Record b
trim = unsafeCoerce

infixr 1 Record.merge as ><
