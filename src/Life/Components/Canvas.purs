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
import Web.HTML (window)
import Web.HTML.Window (requestAnimationFrame)

type Props state =
  { id :: String
  , className :: String
  , height :: Int
  , width :: Int
  , init :: state
  , view :: state -> CanvasElement
  , onMouseDown :: Dispatch MouseEvent state
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

component :: forall state. Eq state => Props state -> ReactElement
component { id, className, height, width, view, init, onMouseDown } = Hooks.component Hooks.do
  stateRef /\ setStateRef <- Hooks.useState Nothing

  Hooks.useEffect $ liftEffect do
    stateRef' <- Ref.new { current: init, previous: Nothing }
    setStateRef $ Just stateRef'

    C.getCanvasElementById id >>= traverse_ \canvas ->
      C.getContext2D canvas >>= renderLoop stateRef'

  Hooks.pure $
    H.canvas_ className
      { id
      , width: show width
      , height: show height
      , style: H.css { width, height }
      , onMouseDown: E.handleEffect \e ->
          for_ stateRef \ref -> do
            state <- Ref.read ref
            state' <- onMouseDown e state.current
            Ref.write state { current = state' } ref
      }
      H.empty
  where
    drawElement ctx = case _ of
      Empty ->
        pure unit
      Fragment elems ->
        foreachE elems $ drawElement ctx
      Rect r -> do
        C.setFillStyle ctx r.fill
        C.fillRect ctx { x: r.x, y: r.y, width: r.width, height: r.height }

    renderLoop stateRef ctx = do
      { current, previous } <- Ref.read stateRef

      when (Just current /= previous) do
        C.clearRect ctx { x: 0.0, y: 0.0, height: Int.toNumber height, width: Int.toNumber width }
        drawElement ctx $ view current
        Ref.write { current, previous: Just current } stateRef

      void $ window >>= requestAnimationFrame (renderLoop stateRef ctx)
