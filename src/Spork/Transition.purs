module Spork.Transition
  ( Transition
  , purely
  ) where

import Data.Monoid (mempty)
import Spork.Batch (Batch)

type Transition m s i =
  { model ∷ s
  , effects ∷ Batch m i
  }

-- | A pure model Transition without effects.
purely ∷ ∀ f s i. s → Transition f s i
purely = { model: _, effects: mempty }
