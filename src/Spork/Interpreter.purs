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
import Data.Functor.Coproduct (Coproduct, coproduct, left, right)
import Data.Newtype (unwrap)
import Spork.EventQueue (EventQueue, Loop(..), stepper)

newtype Interpreter f m = Interpreter (EventQueue m (f (m Unit)))

merge
  ∷ ∀ f g m
  . Applicative m
  ⇒ Interpreter f m
  → Interpreter g m
  → Interpreter (Coproduct f g) m
merge (Interpreter lhs) (Interpreter rhs) = Interpreter \push →
  let
    tick
      ∷ Loop m (f (m Unit))
      → Loop m (g (m Unit))
      → Loop m (Coproduct f g (m Unit))
    tick l r = Loop (update l r) (commit l r)

    update
      ∷ Loop m (f (m Unit))
      → Loop m (g (m Unit))
      → Coproduct f g (m Unit)
      → m (Loop m (Coproduct f g (m Unit)))
    update l@(Loop loopL _) r@(Loop loopR _) =
      coproduct
        (map (flip tick r) <<< loopL)
        (map (tick l) <<< loopR)

    commit
      ∷ Loop m (f (m Unit))
      → Loop m (g (m Unit))
      → Unit
      → m (Loop m (Coproduct f g (m Unit)))
    commit (Loop _ commitL) (Loop _ commitR) _ =
      tick <$> commitL unit <*> commitR unit
  in
    tick
      <$> lhs (push <<< left)
      <*> rhs (push <<< right)

never ∷ ∀ m. Applicative m ⇒ Interpreter (Const Void) m
never = Interpreter (stepper (absurd <<< unwrap))

liftCont ∷ ∀ f m. Applicative m ⇒ (f (m Unit) → m Unit) → Interpreter f m
liftCont = Interpreter <<< stepper

liftNat ∷  ∀ f m. Monad m ⇒ (f ~> m) → Interpreter f m
liftNat k = Interpreter (stepper (join <<< k))

basicEff ∷ ∀ eff. Interpreter (Eff eff) (Eff eff)
basicEff = liftNat id

basicAff ∷ ∀ eff. (Error → Eff eff Unit) → Interpreter (Aff eff) (Eff eff)
basicAff = throughAff id

throughAff ∷ ∀ f eff. (f ~> Aff eff) → (Error → Eff eff Unit) → Interpreter f (Eff eff)
throughAff nat k = liftCont (void <<< runAff (either k id) <<< nat)
