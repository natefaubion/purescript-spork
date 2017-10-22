module Spork.EventQueue
  ( Loop(..)
  , EventQueue
  , EventQueueSpec
  , EventQueueInstance
  , QueueEffects
  , Push
  , stepper
  , fromEventQueueSpec
  , makeEventQueue
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, newRef, writeRef, readRef)
import Control.Monad.Rec.Class as MR
import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)

data Loop m i = Loop (i → m (Loop m i)) (Unit → m (Loop m i))

type Push m i = i → m Unit

type EventQueue m i = Push m i → m (Loop m i)

type QueueEffects eff = (ref ∷ REF | eff)

type EventQueueSpec m s i =
  { init ∷ m s
  , update ∷ s → i → m s
  , commit ∷ s → m s
  }

type EventQueueInstance eff i =
  { run ∷ Eff eff Unit
  , push ∷ Push (Eff eff) i
  }

stepper ∷ ∀ m i. Applicative m ⇒ (i → m Unit) → EventQueue m i
stepper k = tick
  where
  tick ∷ ∀ a. a → m (Loop m i)
  tick _ = pure (Loop loop tick)

  loop ∷ i → m (Loop m i)
  loop a = k a $> Loop loop tick

fromEventQueueSpec
  ∷ ∀ eff s i
  . (Push (Eff eff) i → EventQueueSpec (Eff eff) s i)
  → EventQueue (Eff eff) i
fromEventQueueSpec specFn push =
  let
    spec ∷ EventQueueSpec (Eff eff) s i
    spec = specFn push

    update ∷ s → i → Eff eff (Loop (Eff eff) i)
    update state input = tick <$> spec.update state input

    commit ∷ s → Unit → Eff eff (Loop (Eff eff) i)
    commit state _ = tick <$> spec.commit state

    tick ∷ s → Loop (Eff eff) i
    tick nextState = Loop (update nextState) (commit nextState)
  in
    tick <$> spec.init

makeEventQueue
  ∷ ∀ eff i
  . EventQueue (Eff (QueueEffects eff)) i
  → Eff (QueueEffects eff) (EventQueueInstance (QueueEffects eff) i)
makeEventQueue proc = do
  queue   ← newRef Nothing
  machine ← newRef Nothing

  let
    push ∷ Push (Eff (QueueEffects eff)) i
    push i = do
      q ← readRef queue
      case q of
        Nothing → do
          writeRef queue (Just [i])
          run
        Just is →
          writeRef queue (Just (Array.snoc is i))

    run ∷ Eff (QueueEffects eff) Unit
    run = traverse_ loop =<< readRef machine

    loop ∷ Loop (Eff (QueueEffects eff)) i → Eff (QueueEffects eff) Unit
    loop = MR.tailRecM \(Loop next done) → do
      q ← readRef queue
      case q >>= Array.uncons of
        Just { head, tail } → do
          writeRef queue (Just tail)
          MR.Loop <$> next head
        Nothing → do
          step ← done unit
          isEmpty ← maybe true Array.null <$> readRef queue
          if isEmpty
            then do
              writeRef machine (Just step)
              writeRef queue Nothing
              pure (MR.Done unit)
            else
              pure (MR.Loop step)

  step ← proc push
  writeRef machine (Just step)
  pure { run, push }
