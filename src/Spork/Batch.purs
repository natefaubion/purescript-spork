module Spork.Batch
  ( Batch
  , batch
  , unBatch
  , lift
  ) where

import Prelude

import Data.Functor.Compose (Compose(..))
import Data.Monoid (class Monoid)

newtype Batch f a = Batch (Compose Array f a)

batch ∷ ∀ f a. Array (f a) → Batch f a
batch as = Batch (Compose as)

unBatch ∷ ∀ f a. Batch f a → Array (f a)
unBatch (Batch (Compose as)) = as

lift ∷ ∀ f a. f a → Batch f a
lift = batch <<< pure

derive newtype instance functorBatch ∷ Functor f ⇒ Functor (Batch f)
derive newtype instance applyBatch ∷ Apply f ⇒ Apply (Batch f)
derive newtype instance applicativeBatch ∷ Applicative f ⇒ Applicative (Batch f)

instance semigroupBatch ∷ Semigroup (Batch f a) where
  append (Batch (Compose as)) (Batch (Compose bs)) = Batch (Compose (as <> bs))

instance monoidBatch ∷ Monoid (Batch f a) where
  mempty = Batch (Compose [])
