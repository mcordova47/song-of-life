module Life.Components.FileDrop where

import Prelude

import Effect.Uncurried (EffectFn1)
import Elmish (Dispatch, ReactElement)
import Elmish.Foreign (class CanReceiveFromJavaScript, ValidationResult(..))
import Elmish.HTML (Props_div)
import Elmish.HTML.Events as E
import Elmish.HTML.Internal (StyledTag_)
import Elmish.HTML.Internal as I
import Elmish.React (class ReactChildren)
import Web.Event.Event (preventDefault)
import Web.HTML.Event.DataTransfer (DataTransfer)
import Web.HTML.Event.DragEvent (DragEvent)
import Web.HTML.Event.DragEvent as DE

newtype RDragEvent = RDragEvent DragEvent
instance CanReceiveFromJavaScript RDragEvent where
  validateForeignType _ = Valid

type Props =
  { onDrop :: Dispatch DataTransfer
  }

view :: forall c. ReactChildren c => String -> Props -> c -> ReactElement
view className args =
  div_ className
    { onDragOver: E.handleEffect \(RDragEvent e) ->
        e # DE.toEvent # preventDefault
    , onDrop: E.handleEffect \(RDragEvent e) -> do
        e # DE.toEvent # preventDefault
        args.onDrop $ DE.dataTransfer e
    }

type Props_div' =
  ( onDragOver :: EffectFn1 RDragEvent Unit
  , onDrop :: EffectFn1 RDragEvent Unit
  | Props_div
  )

div_ :: StyledTag_ Props_div'
div_ = I.styledTag_ "div"
