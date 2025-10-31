module Life.HTML.Events.WheelEvent
  ( WheelEvent(..)
  , deltaX
  , deltaY
  , fromEvent
  )
  where

import Prelude

import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Internal.Types (Event)

newtype WheelEvent = WheelEvent Event

type Fields =
  { deltaX :: Number
  , deltaY :: Number
  }

fromEvent :: Event -> WheelEvent
fromEvent = unsafeCoerce

deltaX :: WheelEvent -> Number
deltaX = _.deltaX <<< fields

deltaY :: WheelEvent -> Number
deltaY = _.deltaY <<< fields

fields :: WheelEvent -> Fields
fields = unsafeCoerce
