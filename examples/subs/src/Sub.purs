module Subs.Sub
  ( Coord
  , Sub
  , mouseMove
  , interpreter
  ) where

import Prelude

import Data.Foldable (for_, traverse_)
import Effect (Effect)
import Effect.Ref as Ref
import Spork.EventQueue as EventQueue
import Spork.Interpreter (Interpreter(..))
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener) as DOM
import Web.HTML (window) as DOM
import Web.HTML.Window (toEventTarget) as Window
import Web.UIEvent.MouseEvent (clientX, clientY, fromEvent) as MouseEvent
import Web.UIEvent.MouseEvent.EventTypes (mousemove) as MouseEvent

type Coord =
  { x ∷ Int
  , y ∷ Int
  }

data Sub a = MouseMove (Coord → a)

derive instance functorSub ∷ Functor Sub

mouseMove ∷ ∀ i. (Coord → i) → Sub i
mouseMove = MouseMove

interpreter ∷ ∀ i. Interpreter Effect Sub i
interpreter = Interpreter $ EventQueue.withAccumArray \queue → do
  model ← Ref.new []
  win ← Window.toEventTarget <$> DOM.window
  listener ← DOM.eventListener \event → do
    for_ (MouseEvent.fromEvent event) \mouseEvent → do
      let
        coord =
          { x: MouseEvent.clientX mouseEvent
          , y: MouseEvent.clientY mouseEvent
          }
      Ref.read model >>= traverse_ case _ of
        MouseMove k → queue.push (k coord)
      queue.run

  let
    commit ∷ Array (Sub i) → Effect Unit
    commit new = do
      old ← Ref.read model
      Ref.write new model
      case old, new of
        _,  [] → DOM.removeEventListener MouseEvent.mousemove listener false win
        [], _  → DOM.addEventListener MouseEvent.mousemove listener false win
        _,  _  → pure unit

  pure commit
