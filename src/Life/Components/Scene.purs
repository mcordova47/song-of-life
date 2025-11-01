module Life.Components.Scene where

import Prelude

import Data.Foldable (for_, traverse_)
import Data.Int as Int
import Data.Maybe (Maybe(..), isJust)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect, foreachE)
import Effect.Aff (delay)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Elmish (ReactElement)
import Elmish.HTML.Events (MouseEvent)
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Elmish.Hooks (type (<>), Hook, UseEffect, UseRef, (=/>))
import Elmish.Hooks as Hooks
import Graphics.Canvas as C
import Life.HTML.Events.WheelEvent (WheelEvent)
import Life.HTML.Events.WheelEvent as WheelEvent
import Life.Hooks.UseMutableRef (UseMutableRef, useMutableRef)
import Life.Utils ((:=))
import Life.Utils as U
import Web.DOM.Element (toEventTarget)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListenerWithOptions, eventListener)
import Web.HTML (HTMLCanvasElement, window)
import Web.HTML.HTMLCanvasElement as CE
import Web.HTML.Window (requestAnimationFrame)

data Message
  = Tick Milliseconds
  | MouseDown MouseEvent
  | MouseMove MouseEvent
  | MouseUp MouseEvent
  | Wheel WheelEvent

type Args props state = Args' props state ( render :: (SetState state) -> (String -> ReactElement) -> ReactElement )

type HookArgs props state = Args' props state ()

type Args' props state r =
  { id :: String
  , height :: Int
  , width :: Int
  , fill :: String
  , init :: state
  , props :: props
  , update :: props -> state -> Message -> Effect state
  , view :: state -> Element
  | r
  }

type Dispatch event state = event -> state -> Effect state

data Element
  = Empty
  | Fragment (Array Element)
  | Rect Rect
  | Line Line

instance Semigroup Element where
  append Empty m = m
  append m Empty = m
  append (Fragment f) (Fragment f') = Fragment (f <> f')
  append (Fragment f) m = Fragment (f <> [m])
  append m (Fragment f) = Fragment ([m] <> f)
  append m m' = Fragment [m, m']

instance Monoid Element where
  mempty = Empty

type Rect =
  { position :: Point
  , width :: Number
  , height :: Number
  , fill :: String
  }

type Line =
  { start :: Point
  , end :: Point
  , stroke :: String
  }

type Point =
  { x :: Number
  , y :: Number
  }

type State s =
  { current :: s
  , previous :: Maybe s
  }

type SetState s = (s -> s) -> Effect Unit

type UseScene p s t = UseMutableRef p <> UseMutableRef (State s) <> UseRef HTMLCanvasElement <> UseEffect Unit <> UseEffect Boolean <> UseEffect p <> t

component :: forall props state. Eq props => Eq state => Args props state -> ReactElement
component args =
  useScene (U.trim args) =/> flip args.render

useScene :: forall p s. Eq p => Eq s => HookArgs p s -> Hook (UseScene p s) ((String -> ReactElement) /\ SetState s)
useScene args = Hooks.do
  propsRef <- useMutableRef args.props
  stateRef <- useMutableRef { current: args.init, previous: Nothing }
  canvasElement /\ canvasRef <- Hooks.useRef

  let eventHandler = handleEvent propsRef stateRef

  Hooks.useEffect $ updateLoop propsRef stateRef

  Hooks.useEffect' (isJust canvasElement) \_ -> liftEffect do
    -- wheel event needs to be attached this way because React doesn't support
    -- the `passive: false` option
    for_ canvasElement \canvas -> do
      listener <- eventListener \e -> do
        state <- Ref.read stateRef
        props <- Ref.read propsRef
        state' <- args.update props state.current $ Wheel $ WheelEvent.fromEvent e
        stateRef := state { current = state' }

      canvas # CE.toElement # toEventTarget # addEventListenerWithOptions
        (EventType "wheel")
        listener
        { capture: false, once: false, passive: false }

    C.getCanvasElementById args.id >>= traverse_ \canvas ->
      C.getContext2D canvas >>= renderLoop stateRef

  Hooks.useEffect' args.props (liftEffect <<< (propsRef := _))

  Hooks.pure $
    (flip Tuple)
    (setState stateRef)
    \className ->
      H.canvas_ className
        { id: args.id
        , width: show args.width
        , height: show args.height
        , ref: canvasRef
        , style: H.css { width: args.width, height: args.height }
        , onMouseDown: eventHandler MouseDown
        , onMouseMove: eventHandler MouseMove
        , onMouseUp: eventHandler MouseUp
        }
        H.empty
  where
    clearScene ctx = do
      C.clearRect ctx { x: 0.0, y: 0.0, height: Int.toNumber args.height, width: Int.toNumber args.width }
      C.setFillStyle ctx args.fill
      C.fillRect ctx { x: 0.0, y: 0.0, height: Int.toNumber args.height, width: Int.toNumber args.width }

    drawElement ctx = case _ of
      Empty ->
        pure unit
      Fragment elems ->
        foreachE elems $ drawElement ctx
      Rect { position: { x, y }, width, height, fill } -> do
        C.setFillStyle ctx fill
        C.fillRect ctx { x, y, width, height }
      Line { start, end, stroke } -> do
        C.setStrokeStyle ctx stroke
        C.strokePath ctx do
          C.moveTo ctx start.x start.y
          C.lineTo ctx end.x end.y

    updateLoop propsRef stateRef = do
      let interval = Milliseconds (1000.0 / 60.0)
      liftEffect do
        props <- Ref.read propsRef
        { current, previous } <- Ref.read stateRef
        state <- args.update props current $ Tick interval
        stateRef := { current: state, previous }
      delay interval
      updateLoop propsRef stateRef

    renderLoop stateRef ctx = do
      { current, previous } <- Ref.read stateRef

      when (Just current /= previous) do
        clearScene ctx
        drawElement ctx $ args.view current
        stateRef := { current, previous: Just current }

      void $ window >>= requestAnimationFrame (renderLoop stateRef ctx)

    handleEvent propsRef stateRef handler = E.handleEffect \e -> do
      props <- Ref.read propsRef
      state <- Ref.read stateRef
      state' <- args.update props state.current $ handler e
      stateRef := state { current = state' }

    setState stateRef f = do
      state <- Ref.read stateRef
      stateRef := state { current = f state.current }
