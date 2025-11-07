module Life.Components.Select
  ( Props
  , view
  )
  where

import Prelude

import Data.Maybe (Maybe)
import Elmish (Dispatch, ReactElement, (<?|))
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H

type Props a =
  { decode :: String -> Maybe a
  , display :: a -> String
  , encode :: a -> String
  , onChange :: Dispatch a
  , options :: Array a
  , value :: a
  }

view :: forall a. Props a -> ReactElement
view props =
  H.select_ "form-select"
    { onChange: props.onChange <?| \e ->
        props.decode (E.selectSelectedValue e)
    , value: props.encode props.value
    } $
    props.options <#> \s ->
      H.option_ ""
        { value: props.encode s } $
        props.display s
