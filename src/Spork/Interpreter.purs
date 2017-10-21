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
import Data.Either (Either(..), either)
import Data.Functor.Coproduct (Coproduct(..), left, right)
import Data.Newtype (unwrap)
import Spork.EventQueue (EventQueue, Loop(..), Step(..), Tick, looped)

newtype Interpreter f m = Interpreter (EventQueue m (f (m Unit)))

merge
  ∷ ∀ f g m
  . Applicative m
  ⇒ Interpreter f m
  → Interpreter g m
  → Interpreter (Coproduct f g) m
merge (Interpreter lhs) (Interpreter rhs) = Interpreter \push →
  let
    update
      ∷ Loop m (f (m Unit))
      → Loop m (g (m Unit))
      → Coproduct f g (m Unit)
      → Tick Loop m (Coproduct f g (m Unit))
    update l@(Loop loopL _) r@(Loop loopR _) (Coproduct i) = case i of
      Left iL → loopL iL <#> \nextL → Loop (update nextL r) (commit nextL r)
      Right iR → loopR iR <#> \nextR → Loop (update l nextR) (commit l nextR)

    commit
      ∷ Loop m (f (m Unit))
      → Loop m (g (m Unit))
      → Tick Step m (Coproduct f g (m Unit))
    commit (Loop _ commitL) (Loop _ commitR) =
      map Step $ update
        <$> map looped commitL
        <*> map looped commitR
  in
    map Step $ update
      <$> (map looped $ lhs $ push <<< left)
      <*> (map looped $ rhs $ push <<< right)

never ∷ ∀ m. Applicative m ⇒ Interpreter (Const Void) m
never = Interpreter (const (pure (Step (absurd <<< unwrap))))

liftCont ∷ ∀ f m. Applicative m ⇒ (f (m Unit) → m Unit) → Interpreter f m
liftCont k = Interpreter (const (pure (Step loop)))
  where
  loop ∷ f (m Unit) → Tick Loop m (f (m Unit))
  loop a = k a $> Loop loop (pure (Step loop))

liftNat ∷  ∀ f m. Monad m ⇒ (f ~> m) → Interpreter f m
liftNat k = Interpreter (const (pure (Step loop)))
  where
  loop ∷ f (m Unit) → Tick Loop m (f (m Unit))
  loop a = join (k a) $> Loop loop (pure (Step loop))

basicEff ∷ ∀ eff. Interpreter (Eff eff) (Eff eff)
basicEff = liftNat id

basicAff ∷ ∀ eff. (Error → Eff eff Unit) → Interpreter (Aff eff) (Eff eff)
basicAff = throughAff id

throughAff ∷ ∀ f eff. (f ~> Aff eff) → (Error → Eff eff Unit) → Interpreter f (Eff eff)
throughAff nat k = liftCont (void <<< runAff (either k id) <<< nat)
