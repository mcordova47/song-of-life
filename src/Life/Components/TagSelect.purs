module Life.Components.TagSelect
  ( Props
  , view
  )
  where


import Data.Bounded.Generic (class GenericBottom)
import Data.Codec as C
import Data.Enum.Generic (class GenericBoundedEnum, class GenericEnum)
import Data.Generic.Rep (class Generic)
import Elmish (Dispatch, ReactElement)
import Life.Components.Select as Select
import Life.Types.Codec (class Serializable, codec)
import Life.Utils.Generic as G

type Props a =
  { display :: a -> String
  , onChange :: Dispatch a
  , value :: a
  }

view :: forall @a rep
  . Generic a rep
  => GenericBottom rep
  => GenericEnum rep
  => GenericBoundedEnum rep
  => Serializable a
  => Props a
  -> ReactElement
view { display, onChange, value } =
  Select.view
    { decode: C.decode codec
    , display
    , encode: C.encode codec
    , onChange
    , options: G.tags
    , value
    }
