module Spork.Interpreter
  ( Interpreter(..)
  , merge
  , never
  , liftNat
  , liftCont
  , basicEff
  , basicAff
  , throughAff
  ) where

import Prelude

import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Data.Const (Const)
import Data.Either (either)
import Data.Functor.Coproduct (Coproduct, coproduct)
import Data.Newtype (unwrap)
import Spork.EventQueue (EventQueue, Loop(..), stepper, withCont)

-- | An Interpreter takes inputs embedded in some functor `f` and pushes them
-- | out with effects `m`.
newtype Interpreter m f i = Interpreter (EventQueue m (f i) i)

-- | Funnels two Interpreters into one by taking the Coproduct.
merge
  ∷ ∀ f g m i
  . Applicative m
  ⇒ Interpreter m f i
  → Interpreter m g i
  → Interpreter m (Coproduct f g) i
merge (Interpreter lhs) (Interpreter rhs) = Interpreter \queue →
  let
    tick ∷ Loop m (f i) → Loop m (g i) → Loop m (Coproduct f g i)
    tick l r = Loop (update l r) (commit l r)

    update ∷ Loop m (f i) → Loop m (g i) → Coproduct f g i → m (Loop m (Coproduct f g i))
    update l@(Loop loopL _) r@(Loop loopR _) =
      coproduct
        (map (flip tick r) <<< loopL)
        (map (tick l) <<< loopR)

    commit ∷ Loop m (f i) → Loop m (g i) → Unit → m (Loop m (Coproduct f g i))
    commit (Loop _ commitL) (Loop _ commitR) _ =
      tick <$> commitL unit <*> commitR unit
  in
    tick <$> lhs queue <*> rhs queue

-- | The Void interpreter.
never ∷ ∀ m i. Monad m ⇒ Interpreter m (Const Void) i
never = Interpreter (stepper (absurd <<< unwrap))

-- | Builds an Interpreter from a natural transformation.
liftNat ∷  ∀ f m i. Monad m ⇒ (f ~> m) → Interpreter m f i
liftNat k = Interpreter (stepper k)

-- | Runs `Eff` effects.
basicEff ∷ ∀ eff i. Interpreter (Eff eff) (Eff eff) i
basicEff = liftNat id

-- | Builds an Interpreter from callbacks.
liftCont ∷ ∀ f m i. Applicative m ⇒ (∀ j. (j → m Unit) → f j → m Unit) → Interpreter m f i
liftCont k =  Interpreter (withCont \queue → k \j → queue.push j *> queue.run)

-- | Runs `Aff` effects. Takes a callback for handling exceptions.`
basicAff ∷ ∀ eff i. (Error → Eff eff Unit) → Interpreter (Eff eff) (Aff eff) i
basicAff = throughAff id

-- | Interprets some functor `f` into `Aff` before running it.
throughAff ∷ ∀ eff f i. (f ~> Aff eff) → (Error → Eff eff Unit) → Interpreter (Eff eff) f i
throughAff nat err = liftCont (\k → void <<< runAff (either err k) <<< nat)
