module Life.Hooks.UseBoundingBox where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (liftEffect)
import Elmish as Elmish
import Elmish.Hooks (type (<>), Hook, UseEffect, UseState, UseRef)
import Elmish.Hooks as Hooks
import Web.DOM.Element (DOMRect, Element)
import Web.DOM.Element as Elem
import Web.HTML.HTMLDivElement (HTMLDivElement)
import Web.HTML.HTMLDivElement as Div

type UseBoundingBox el t = UseState (Maybe DOMRect) <> UseRef el <> UseEffect Boolean <> t

useBoundingBox :: Hook (UseBoundingBox HTMLDivElement) (Maybe DOMRect /\ Elmish.Ref HTMLDivElement)
useBoundingBox = useBoundingBox' Div.toElement

useBoundingBox' :: forall el. (el -> Element) -> Hook (UseBoundingBox el) (Maybe DOMRect /\ Elmish.Ref el)
useBoundingBox' toElement = Hooks.do
  box /\ setBox <- Hooks.useState Nothing
  elem /\ ref <- Hooks.useRef

  Hooks.useEffect' (isJust elem) \_ -> liftEffect do
    for_ elem \el -> do
      el
        # toElement
        # Elem.getBoundingClientRect
        <#> Just
        <#> setBox

  Hooks.pure (box /\ ref)
