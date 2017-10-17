module Spork.EventQueue
  ( Tick
  , Step(..)
  , Loop(..)
  , EventQueue
  , EventQueueSpec
  , QueueEffects
  , Push
  , makeEventQueue
  , runEventQueue
  , looped
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, newRef, writeRef, readRef)
import Control.Monad.Rec.Class as MR
import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)

type Tick f m i = m (f m i)

newtype Step m i = Step (i → Tick Loop m i)

data Loop m i = Loop (i → Tick Loop m i) (Tick Step m i)

type Push m i = i → m Unit

type EventQueue m i = Push m i → Tick Step m i

type QueueEffects eff = (ref ∷ REF | eff)

derive instance newtypeStep ∷ Newtype (Step m i) _

looped ∷ ∀ m i. Applicative m ⇒ Step m i → Loop m i
looped step = Loop (unwrap step) (pure step)

type EventQueueSpec m s i =
  { init ∷ m s
  , tick ∷ s → i → m s
  , flush ∷ s → m s
  }

makeEventQueue
  ∷ ∀ eff s i
  . (Push (Eff eff) i → EventQueueSpec (Eff eff) s i)
  → EventQueue (Eff eff) i
makeEventQueue specFn push =
  let
    spec ∷ EventQueueSpec (Eff eff) s i
    spec = specFn push

    tick ∷ s → i → Tick Loop (Eff eff) i
    tick state input = do
      nextState ← spec.tick state input
      pure $ Loop (tick nextState) (flush nextState)

    flush ∷ s → Tick Step (Eff eff) i
    flush state = Step <<< tick <$> spec.flush state
  in
    Step <<< tick <$> spec.init

runEventQueue
  ∷ ∀ eff i
  . EventQueue (Eff (QueueEffects eff)) i
  → Eff (QueueEffects eff) (Push (Eff (QueueEffects eff)) i)
runEventQueue proc = do
  queue   ← newRef Nothing
  machine ← newRef Nothing

  let
    push ∷ Push (Eff (QueueEffects eff)) i
    push i = do
      q ← readRef queue
      case q of
        Nothing → do
          writeRef queue (Just [i])
          start
        Just is →
          writeRef queue (Just (Array.snoc is i))

    start ∷ Eff (QueueEffects eff) Unit
    start = do
      step ← readRef machine
      for_ step (loop <<< startLoop)

    startLoop ∷ Step (Eff (QueueEffects eff)) i → Loop (Eff (QueueEffects eff)) i
    startLoop (Step fn) = Loop fn (pure (Step fn))

    loop ∷ Loop (Eff (QueueEffects eff)) i → Eff (QueueEffects eff) Unit
    loop = MR.tailRecM \(Loop next done) → do
      q ← readRef queue
      case q >>= Array.uncons of
        Just { head, tail } → do
          writeRef queue (Just tail)
          MR.Loop <$> next head
        Nothing → do
          step ← done
          isEmpty ← maybe true Array.null <$> readRef queue
          if isEmpty
            then do
              writeRef machine (Just step)
              writeRef queue Nothing
              pure (MR.Done unit)
            else
              pure (MR.Loop (startLoop step))

  step ← proc push
  writeRef machine (Just step)
  start
  pure push
