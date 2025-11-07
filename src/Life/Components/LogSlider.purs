module Life.Components.LogSlider where

import Prelude

import Data.Int as Int
import Data.Number as Number
import Elmish (ReactElement, Dispatch, (<?|))
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H

type Props =
  { id :: String
  , max :: Number
  , min :: Number
  , onChange :: Dispatch Number
  , value :: Number
  }

view :: String -> Props -> ReactElement
view className props =
  H.input_ ("form-range " <> className)
    { type: "range"
    , min: show min
    , max: show max
    , step: "1"
    , value: show value
    , onChange: props.onChange <?| map fromLog <<< Int.fromString <<< E.inputText
    , id: props.id
    }
  where
    fromLog v = Number.pow 10.0 (Int.toNumber v / 50.0)
    toLog v = Int.floor (Number.log v * 50.0 / Number.ln10)
    value = toLog props.value
    min = toLog props.min
    max = toLog props.max
