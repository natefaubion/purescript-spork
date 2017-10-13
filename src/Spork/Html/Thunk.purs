module Spork.Html.Thunk
  ( Thunk
  , buildThunk
  , runThunk
  , unsafeEqThunk
  , thunked
  , thunk1
  , thunk2
  , thunk3
  ) where

import Prelude
import Data.Function.Uncurried as Fn
import DOM (DOM)
import DOM.Node.Types (Node) as DOM
import Halogen.VDom as V
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (reallyUnsafeRefEq)

foreign import data TArg ∷ Type

foreign import data TId ∷ Type

type ThunkR f j i =
  { mapk ∷ f i → f j
  , id ∷ TId
  , eq ∷ TArg → TArg → Boolean
  , arg ∷ TArg
  , render ∷ TArg → f i
  }

foreign import data Thunk ∷ (Type → Type) → Type → Type

unThunk ∷ ∀ f j r. (∀ i. ThunkR f j i → r) → Thunk f j → r
unThunk = unsafeCoerce

toThunk ∷ ∀ f j i. ThunkR f j i → Thunk f j
toThunk = unsafeCoerce

unsafeTId ∷ ∀ a. a → TId
unsafeTId = unsafeCoerce

instance functorThunk ∷ Functor f ⇒ Functor (Thunk f) where
  map f = unThunk \tr → toThunk (tr { mapk = tr.mapk >>> map f})

thunk ∷ ∀ a f i. Fn.Fn4 TId (a → a → Boolean) (a → f i) a (Thunk f i)
thunk = Fn.mkFn4 \tid eqFn f a → toThunk
  { mapk: id
  , id: tid
  , eq: unsafeCoerce eqFn ∷ TArg → TArg → Boolean
  , render: unsafeCoerce f ∷ TArg → f i
  , arg: unsafeCoerce a ∷ TArg
  }

thunked ∷ ∀ a f i. (a → a → Boolean) → (a → f i) → a → Thunk f i
thunked eqFn f = let tid = unsafeTId { f } in \a → Fn.runFn4 thunk tid eqFn f a

thunk1 ∷ ∀ a f i. Fn.Fn2 (a → f i) a (Thunk f i)
thunk1 = Fn.mkFn2 \f a → Fn.runFn4 thunk (unsafeTId f) reallyUnsafeRefEq f a

thunk2 ∷ ∀ a b f i. Fn.Fn3 (a → b → f i) a b (Thunk f i)
thunk2 =
  let
    comp a b =
      reallyUnsafeRefEq a._1 b._1 && reallyUnsafeRefEq a._2 b._2
  in Fn.mkFn3 \f a b →
    Fn.runFn4 thunk (unsafeTId f) comp (\{ _1, _2 } → f _1 _2) { _1: a, _2: b }

thunk3 ∷ ∀ a b c f i. Fn.Fn4 (a → b → c → f i) a b c (Thunk f i)
thunk3 =
  let
    comp a b =
      reallyUnsafeRefEq a._1 b._1 && reallyUnsafeRefEq a._2 b._2 && reallyUnsafeRefEq a._3 b._3
  in Fn.mkFn4 \f a b c →
    Fn.runFn4 thunk (unsafeTId f) comp (\{ _1, _2, _3 } → f _1 _2 _3) { _1: a, _2: b, _3: c }

runThunk ∷ ∀ f i. Thunk f i → f i
runThunk = unThunk \tr → tr.mapk (tr.render tr.arg)

unsafeEqThunk ∷ ∀ f i. Thunk f i → Thunk f i → Boolean
unsafeEqThunk = unThunk \tr1 → unThunk \tr2 →
  reallyUnsafeRefEq tr1.eq tr2.eq && reallyUnsafeRefEq tr1.id tr2.id && tr2.eq tr1.arg tr2.arg

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
