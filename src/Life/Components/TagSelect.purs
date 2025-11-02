module Life.Components.TagSelect
  ( Props
  , view
  )
  where

import Prelude

import Data.Bounded.Generic (class GenericBottom)
import Data.Codec as C
import Data.Enum.Generic (class GenericBoundedEnum, class GenericEnum)
import Data.Generic.Rep (class Generic)
import Elmish (Dispatch, ReactElement, (<?|))
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
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
  H.select_ "form-select"
    { onChange: onChange <?| \e ->
        C.decode codec (E.selectSelectedValue e)
    , value: C.encode codec value
    } $
    (G.tags :: Array _) <#> \s ->
      H.option_ ""
        { value: C.encode codec s } $
        display s
