module Spork.EventQueue
  ( Loop(..)
  , EventQueue
  , EventQueueAccum
  , EventQueueInstance
  , stepper
  , withCont
  , withAccum
  , withAccumArray
  , fix
  ) where

import Prelude

import Control.Monad.Rec.Class as MR
import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref as Ref

data Loop m i = Loop (i → m (Loop m i)) (Unit → m (Loop m i))

type EventQueue m i o = EventQueueInstance m o → m (Loop m i)

type EventQueueAccum m s i =
  { init ∷ s
  , update ∷ s → i → m s
  , commit ∷ s → m s
  }

type EventQueueInstance m o =
  { run ∷ m Unit
  , push ∷ o → m Unit
  }

-- | Creates an EventQueue from a Kleisli arrow.
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

-- | Creates an EventQueue which performs an action on every input.
withCont ∷ ∀ m i o. Applicative m ⇒ (EventQueueInstance m o → i → m Unit) → EventQueue m i o
withCont k next = tick unit
  where
  push ∷ i → m Unit
  push = k next

  tick ∷ Unit → m (Loop m i)
  tick _ = pure (Loop loop tick)

  loop ∷ i → m (Loop m i)
  loop i = push i $> Loop loop tick

-- | Creates an EventQueue with an EventQueueAccum lifecycle.
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

-- | Creates an EventQueue comprised of only a commit action for inputs
-- | buffered in an Array.
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

-- | Creates an EventQueue that can feed back into itself.
fix
  ∷ ∀ i
  . EventQueue Effect i i
  → Effect (EventQueueInstance Effect i)
fix proc = do
  queue   ← Ref.new []
  machine ← Ref.new Nothing

  let
    push ∷ i → Effect Unit
    push = flip Ref.modify_ queue <<< flip Array.snoc

    run ∷ Effect Unit
    run = traverse_ loop =<< (Ref.read machine <* Ref.write Nothing machine)

    loop ∷ Loop Effect i → Effect Unit
    loop = MR.tailRecM \(Loop next done) → do
      q ← Ref.read queue
      case Array.uncons q of
        Just { head, tail } → do
          Ref.write tail queue
          MR.Loop <$> next head
        Nothing → do
          step ← done unit
          isEmpty ← Array.null <$> Ref.read queue
          if isEmpty
            then do
              Ref.write (Just step) machine
              Ref.write [] queue
              pure (MR.Done unit)
            else
              pure (MR.Loop step)

    inst ∷ EventQueueInstance Effect i
    inst = { run, push }

  step ← proc inst
  Ref.write (Just step) machine
  pure inst
