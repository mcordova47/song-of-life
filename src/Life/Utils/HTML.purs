module Life.Utils.HTML where


import Elmish.Foreign (class CanReceiveFromJavaScript, ValidationResult(..))
import Elmish.HTML (Props_div)
import Elmish.HTML.Events (EventHandler)
import Elmish.HTML.Internal (StyledTag_, styledTag_)
import Web.Clipboard.ClipboardEvent (ClipboardEvent)

type Props_div' =
  ( onPaste :: EventHandler RClipboardEvent
  | Props_div
  )

newtype RClipboardEvent = RClipboardEvent ClipboardEvent
instance CanReceiveFromJavaScript RClipboardEvent where
  validateForeignType _ = Valid

div_ :: StyledTag_ Props_div'
div_ = styledTag_ "div"
