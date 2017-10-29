module Spork.EventQueue
  ( Loop(..)
  , EventQueue
  , EventQueueAccum
  , EventQueueInstance
  , QueueEffects
  , stepper
  , withCont
  , withAccum
  , withAccumArray
  , fix
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef, modifyRef)
import Control.Monad.Rec.Class as MR
import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))

data Loop m i = Loop (i → m (Loop m i)) (Unit → m (Loop m i))

type EventQueue m i o = EventQueueInstance m o → m (Loop m i)

type QueueEffects eff = (ref ∷ REF | eff)

type EventQueueAccum m s i =
  { init ∷ s
  , update ∷ s → i → m s
  , commit ∷ s → m s
  }

type EventQueueInstance m o =
  { run ∷ m Unit
  , push ∷ o → m Unit
  }

stepper ∷ ∀ m i o. Monad m ⇒ (i → m o) → EventQueue m i o
stepper k next = tick unit
  where
  tick ∷ Unit → m (Loop m i)
  tick _ = pure (Loop loop tick)

  loop ∷ i → m (Loop m i)
  loop i = do
    next.push =<< k i
    next.run
    pure (Loop loop tick)

withCont ∷ ∀ m i o. Applicative m ⇒ (EventQueueInstance m o → i → m Unit) → EventQueue m i o
withCont k next = tick unit
  where
  push ∷ i → m Unit
  push = k next

  tick ∷ Unit → m (Loop m i)
  tick _ = pure (Loop loop tick)

  loop ∷ i → m (Loop m i)
  loop i = push i $> Loop loop tick

withAccum
  ∷ ∀ m s i o
  . Applicative m
  ⇒ (EventQueueInstance m o → m (EventQueueAccum m s i))
  → EventQueue m i o
withAccum specFn next = specFn next <#> \spec →
  let
    tick ∷ s → Loop m i
    tick nextState = Loop (update nextState) (commit nextState)

    update ∷ s → i → m (Loop m i)
    update state input = tick <$> spec.update state input

    commit ∷ s → Unit → m (Loop m i)
    commit state _ = tick <$> spec.commit state
  in
    tick spec.init

withAccumArray
  ∷ ∀ m i o
  . Applicative m
  ⇒ (EventQueueInstance m o → m (Array i → m Unit))
  → EventQueue m i o
withAccumArray specFn = withAccum \next → specFn next <#> \spec →
  let
    update ∷ Array i → i → m (Array i)
    update buffer i = pure (Array.snoc buffer i)

    commit ∷ Array i → m (Array i)
    commit buffer = spec buffer $> []
  in
    { init: [], commit, update }

fix
  ∷ ∀ eff i
  . EventQueue (Eff (QueueEffects eff)) i i
  → Eff (QueueEffects eff) (EventQueueInstance (Eff (QueueEffects eff)) i)
fix proc = do
  queue   ← newRef []
  machine ← newRef Nothing

  let
    push ∷ i → Eff (QueueEffects eff) Unit
    push = modifyRef queue <<< flip Array.snoc

    run ∷ Eff (QueueEffects eff) Unit
    run = traverse_ loop =<< (readRef machine <* writeRef machine Nothing)

    loop ∷ Loop (Eff (QueueEffects eff)) i → Eff (QueueEffects eff) Unit
    loop = MR.tailRecM \(Loop next done) → do
      q ← readRef queue
      case Array.uncons q of
        Just { head, tail } → do
          writeRef queue tail
          MR.Loop <$> next head
        Nothing → do
          step ← done unit
          isEmpty ← Array.null <$> readRef queue
          if isEmpty
            then do
              writeRef machine (Just step)
              writeRef queue []
              pure (MR.Done unit)
            else
              pure (MR.Loop step)

    inst ∷ EventQueueInstance (Eff (QueueEffects eff)) i
    inst = { run, push }

  step ← proc inst
  writeRef machine (Just step)
  pure inst
