module Subs.Sub
  ( Coord
  , Sub
  , SubEffects
  , mouseMove
  , interpreter
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.EventTarget (EventListener, addEventListener, eventListener, removeEventListener) as DOM
import DOM.Event.MouseEvent (clientX, clientY, eventToMouseEvent) as DOM
import DOM.HTML (window) as DOM
import DOM.HTML.Event.EventTypes (mousemove) as DOM
import DOM.HTML.Types (windowToEventTarget) as DOM
import Data.Foldable (for_, traverse_)
import Spork.EventQueue as EventQueue
import Spork.Interpreter (Interpreter(..))

type Coord =
  { x ∷ Int
  , y ∷ Int
  }

data Sub a = MouseMove (Coord → a)

derive instance functorSub ∷ Functor Sub

mouseMove ∷ ∀ i. (Coord → i) → Sub i
mouseMove = MouseMove

type SubEffects eff =
  ( ref ∷ REF
  , dom ∷ DOM
  | eff
  )

interpreter ∷ ∀ eff i. Interpreter (Eff (SubEffects eff)) Sub i
interpreter = Interpreter $ EventQueue.withAccumArray \queue → do
  model ← newRef []
  win ← DOM.windowToEventTarget <$> DOM.window

  let
    listener ∷ DOM.EventListener (SubEffects eff)
    listener = DOM.eventListener \event → do
      for_ (runExcept $ DOM.eventToMouseEvent event) \mouseEvent → do
        let
          coord =
            { x: DOM.clientX mouseEvent
            , y: DOM.clientY mouseEvent
            }
        readRef model >>= traverse_ case _ of
          MouseMove k → queue.push (k coord)
        queue.run

    commit ∷ Array (Sub i) → Eff (SubEffects eff) Unit
    commit new = do
      old ← readRef model
      writeRef model new
      case old, new of
        _,  [] → DOM.removeEventListener DOM.mousemove listener false win
        [], _  → DOM.addEventListener DOM.mousemove listener false win
        _,  _  → pure unit

  pure commit
