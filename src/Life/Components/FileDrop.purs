module Life.Components.FileDrop
  ( Props
  , view
  )
  where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Elmish (ReactElement, Dispatch)
import Elmish.Foreign (class CanReceiveFromJavaScript, ValidationResult(..))
import Elmish.HTML (Props_label)
import Elmish.HTML.Events (InputChangeEvent(..))
import Elmish.HTML.Events as E
import Elmish.HTML.Internal (StyledTag_)
import Elmish.HTML.Internal as I
import Elmish.HTML.Styled as H
import Elmish.React (class ReactChildren, asReactChildren)
import Web.Event.Event (preventDefault)
import Web.File.File (File)
import Web.File.FileList as FL
import Web.HTML.Event.DataTransfer as DT
import Web.HTML.Event.DragEvent (DragEvent)
import Web.HTML.Event.DragEvent as DE
import Web.HTML.HTMLInputElement as IE

newtype RDragEvent = RDragEvent DragEvent
instance CanReceiveFromJavaScript RDragEvent where
  validateForeignType _ = Valid

type Props =
  { handleData :: Dispatch (MediaType -> Effect String)
  , handleFile :: Dispatch File
  }

view :: forall c. ReactChildren c => String -> Props -> c -> ReactElement
view className args content =
  label_ className
    { onDragOver: E.handleEffect \(RDragEvent e) ->
        e # DE.toEvent # preventDefault
    , onDrop: E.handleEffect \(RDragEvent e) -> do
        e # DE.toEvent # preventDefault
        let dataTransfer = DE.dataTransfer e
        case dataTransfer # DT.files >>= FL.item 0 of
          Just file -> args.handleFile file
          Nothing -> args.handleData $ flip DT.getData dataTransfer
    }
    [ H.fragment $ asReactChildren content
    , H.input_ "d-none"
        { type: "file"
        , onChange: E.handleEffect \(InputChangeEvent e) ->
            for_ (e.target # IE.fromElement) \inputElement -> do
              files <- inputElement # IE.files
              for_ (files >>= FL.item 0) args.handleFile
        }
    ]

type Props_label' =
  ( onDragOver :: EffectFn1 RDragEvent Unit
  , onDrop :: EffectFn1 RDragEvent Unit
  | Props_label
  )

label_ :: StyledTag_ Props_label'
label_ = I.styledTag_ "label"
