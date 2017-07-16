module Spork.Html.Thunk
  ( Thunk
  , buildThunk
  , runThunk
  , unsafeEqThunk
  , thunk1
  , thunk2
  , thunk3
  ) where

import Prelude
import Data.Exists (Exists, mkExists, runExists)
import Data.Function.Uncurried as Fn
import DOM (DOM)
import DOM.Node.Types (Node) as DOM
import Halogen.VDom as V
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (reallyUnsafeRefEq)

foreign import data TArg ∷ Type

data Thunk' f j i = Thunk' (f i → f j) (TArg → TArg → Boolean) (TArg → f i) TArg

newtype Thunk f i = Thunk (Exists (Thunk' f i))

runExists' ∷ ∀ f r. Exists f → (∀ a. f a → r) → r
runExists' = flip (unsafeCoerce runExists)

instance functorThunk ∷ Functor f ⇒ Functor (Thunk f) where
  map f (Thunk ex) =
    runExists' ex case _ of
      Thunk' g r c a1 → Thunk (mkExists (Thunk' (g >>> map f) r c a1))

thunk ∷ ∀ a f i. Fn.Fn3 (a → a → Boolean) (a → f i) a (Thunk f i)
thunk = Fn.mkFn3 \comp f a →
  Thunk
    (mkExists
      (Thunk' id
        (unsafeCoerce comp)
        (unsafeCoerce f)
        (unsafeCoerce a)))

thunk1 ∷ ∀ a f i. Fn.Fn2 (a → f i) a (Thunk f i)
thunk1 = Fn.mkFn2 \f a → Fn.runFn3 thunk reallyUnsafeRefEq f a

thunk2 ∷ ∀ a b f i. Fn.Fn3 (a → b → f i) a b (Thunk f i)
thunk2 =
  let
    comp a b =
      reallyUnsafeRefEq a._1 b._1 && reallyUnsafeRefEq a._2 b._2
  in Fn.mkFn3 \f a b →
    Fn.runFn3 thunk comp (\{ _1, _2 } → f _1 _2) { _1: a, _2: b }

thunk3 ∷ ∀ a b c f i. Fn.Fn4 (a → b → c → f i) a b c (Thunk f i)
thunk3 =
  let
    comp a b =
      reallyUnsafeRefEq a._1 b._1 && reallyUnsafeRefEq a._2 b._2 && reallyUnsafeRefEq a._3 b._3
  in Fn.mkFn4 \f a b c →
    Fn.runFn3 thunk comp (\{ _1, _2, _3 } → f _1 _2 _3) { _1: a, _2: b, _3: c }

runThunk ∷ ∀ f i. Thunk f i → f i
runThunk (Thunk ex) =
  runExists' ex case _ of
    Thunk' g _ r a1 → g (r a1)

unsafeEqThunk ∷ ∀ f i. Thunk f i → Thunk f i → Boolean
unsafeEqThunk (Thunk a) (Thunk b) =
  runExists' a \t1 →
    runExists' b \t2 → case t1, t2 of
      Thunk' _ c r arg, Thunk' _ c' r' arg' →
        reallyUnsafeRefEq c c' && reallyUnsafeRefEq r r' && c' arg arg'

buildThunk
  ∷ ∀ eff f i a w
  . (f i → V.VDom a w)
  → V.VDomSpec (dom ∷ DOM | eff) a w
  → V.VDomMachine (dom ∷ DOM | eff) (Thunk f i) DOM.Node
buildThunk toVDom spec = render
  where
    render t = do
      res@V.Step n _ h ← V.buildVDom spec (toVDom (runThunk t))
      pure (V.Step n (patch res t) h)

    patch (prev@V.Step n step h) t t' = do
      if unsafeEqThunk t t'
        then pure (V.Step n (patch prev t) h)
        else do
          res@V.Step n' _ h' ← step (toVDom (runThunk t'))
          pure (V.Step n' (patch res t') h')
