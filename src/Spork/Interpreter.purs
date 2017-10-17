module Spork.Interpreter
  ( Interpreter(..)
  , pureInterpreter
  , basicEff
  , toBasicEff
  , basicAff
  , toBasicAff
  ) where

import Prelude

import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Data.Const (Const)
import Data.Either (either)
import Data.Functor.Coproduct (Coproduct, coproduct)
import Data.Newtype (unwrap)
import Spork.EventQueue (EventQueue, makeEventQueue)

newtype Interpreter eff m q = Interpreter (EventQueue (Eff eff) (Coproduct m q (Eff eff Unit)))

basicInterpreter
  ∷ ∀ eff m
  . (m (Eff eff Unit) → Eff eff Unit)
  → Interpreter eff m (Const Void)
basicInterpreter onEffect = Interpreter $ makeEventQueue $ const
  { init: pure unit
  , tick: const $ coproduct onEffect (absurd <<< unwrap)
  , flush: pure
  }

pureInterpreter ∷ ∀ eff. Interpreter eff (Const Void) (Const Void)
pureInterpreter = basicInterpreter (absurd <<< unwrap)

basicEff ∷ ∀ eff. Interpreter eff (Eff eff) (Const Void)
basicEff = toBasicEff id

toBasicEff ∷ ∀ eff f. (f ~> Eff eff) → Interpreter eff f (Const Void)
toBasicEff nat = basicInterpreter (join <<< nat)

basicAff ∷ ∀ eff. (Error → Eff eff Unit) → Interpreter eff (Aff eff) (Const Void)
basicAff = toBasicAff id

toBasicAff ∷ ∀ eff f. (f ~> Aff eff) → (Error → Eff eff Unit) → Interpreter eff f (Const Void)
toBasicAff nat onError = basicInterpreter (void <<< runAff (either onError id) <<< nat)
