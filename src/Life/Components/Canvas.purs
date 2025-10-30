module Life.Components.Canvas where

import Prelude

import Data.Foldable (for_, traverse_)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect, foreachE)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Elmish (ReactElement)
import Elmish.HTML.Events (MouseEvent)
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Elmish.Hooks as Hooks
import Graphics.Canvas as C
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (toEventTarget)
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event (EventType(..), preventDefault, stopPropagation)
import Web.Event.EventTarget (addEventListenerWithOptions, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument as Doc
import Web.HTML.Window (document, requestAnimationFrame)

data Message
  = Tick
  | MouseDown MouseEvent
  | MouseMove MouseEvent
  | MouseUp MouseEvent
  | Wheel WheelEvent

newtype WheelEvent = WheelEvent { deltaY :: Int }

type Props props state =
  { id :: String
  , height :: Int
  , width :: Int
  , fill :: String
  , init :: state
  , props :: props
  , update :: props -> state -> Message -> Effect state
  , view :: state -> CanvasElement
  , render :: ((state -> state) -> Effect Unit) -> ReactElement -> ReactElement
  }

type Dispatch event state = event -> state -> Effect state

data CanvasElement
  = Empty
  | Fragment (Array CanvasElement)
  | Rect Rect

instance Semigroup CanvasElement where
  append Empty m = m
  append m Empty = m
  append (Fragment f) (Fragment f') = Fragment (f <> f')
  append (Fragment f) m = Fragment (f <> [m])
  append m (Fragment f) = Fragment ([m] <> f)
  append m m' = Fragment [m, m']

instance Monoid CanvasElement where
  mempty = Empty

type Rect =
  { x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  , fill :: String
  }

component :: forall props state. Eq props => Eq state => String -> Props props state -> ReactElement
component className props = Hooks.component Hooks.do
  stateRef /\ setStateRef <- Hooks.useState Nothing
  propsRef /\ setPropsRef <- Hooks.useState Nothing
  let
    eventHandler = handleEvent propsRef stateRef

    setState f =
      for_ stateRef \ref -> do
        state <- Ref.read ref
        Ref.write state { current = f state.current } ref

    setProps p' =
      for_ propsRef $ Ref.write p'

  Hooks.useEffect $ liftEffect do
    stateRef' <- Ref.new { current: props.init, previous: Nothing }
    setStateRef $ Just stateRef'

    propsRef' <- Ref.new props.props
    setPropsRef $ Just propsRef'

    mCanvas <- window >>= document <#> Doc.toNonElementParentNode >>= getElementById "canvas"
    for_ mCanvas \canvas -> do
      listener <- eventListener \e -> do
        preventDefault e
        stopPropagation e
        state <- Ref.read stateRef'
        p <- Ref.read propsRef'
        state' <- props.update p state.current $ Wheel $ WheelEvent $ unsafeCoerce e
        Ref.write state { current = state' } stateRef'

      canvas # toEventTarget # addEventListenerWithOptions (EventType "wheel") listener { capture: false, once: false, passive: false }

    C.getCanvasElementById props.id >>= traverse_ \canvas ->
      C.getContext2D canvas >>= renderLoop propsRef' stateRef'

  Hooks.useEffect' props.props (liftEffect <<< setProps)

  Hooks.pure $
    props.render setState $
      H.canvas_ className
        { id: props.id
        , width: show props.width
        , height: show props.height
        , style: H.css { width: props.width, height: props.height }
        , onMouseDown: eventHandler MouseDown
        , onMouseMove: eventHandler MouseMove
        , onMouseUp: eventHandler MouseUp
        }
        H.empty
  where
    clearCanvas ctx = do
      C.clearRect ctx { x: 0.0, y: 0.0, height: Int.toNumber props.height, width: Int.toNumber props.width }
      C.setFillStyle ctx props.fill
      C.fillRect ctx { x: 0.0, y: 0.0, height: Int.toNumber props.height, width: Int.toNumber props.width }

    drawElement ctx = case _ of
      Empty ->
        pure unit
      Fragment elems ->
        foreachE elems $ drawElement ctx
      Rect r -> do
        C.setFillStyle ctx r.fill
        C.fillRect ctx { x: r.x, y: r.y, width: r.width, height: r.height }

    renderLoop propsRef stateRef ctx = do
      p <- Ref.read propsRef
      { current, previous } <- Ref.read stateRef
      state <- props.update p current Tick

      when (Just state /= previous) do
        clearCanvas ctx
        drawElement ctx $ props.view state
        Ref.write { current: state, previous: Just state } stateRef

      void $ window >>= requestAnimationFrame (renderLoop propsRef stateRef ctx)

    handleEvent propsRef stateRef handler = E.handleEffect \e ->
      for_ ((/\) <$> propsRef <*> stateRef) \(pr /\ sr) -> do
        p <- Ref.read pr
        state <- Ref.read sr
        state' <- props.update p state.current $ handler e
        Ref.write state { current = state' } sr
