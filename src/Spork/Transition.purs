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

purely ∷ ∀ f s i. s → Transition f s i
purely = { model: _, effects: mempty }
