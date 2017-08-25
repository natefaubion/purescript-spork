module Spork.App
  ( App
  , AppEffects
  , PureApp
  , BasicApp
  , Transition
  , Batch
  , batch
  , unBatch
  , exec
  , subscribe
  , purely
  , Push
  , InterpreterSpec
  , Interpreter(..)
  , Step(..)
  , Loop(..)
  , EventQueue
  , runEventQueue
  , run
  , runWithSelector
  , makeInterpreter
  , basicInterpreter
  , pureInterpreter
  , toBasicEff
  , toBasicAff
  , basicEff
  , basicAff
  ) where

import Prelude
import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, Error, throwException, error)
import Control.Monad.Eff.Ref (REF, newRef, writeRef, readRef)
import Control.Monad.Rec.Class as MR
import Data.Array as Array
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Function.Uncurried (runFn2)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Coproduct (Coproduct(..), coproduct, left, right)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), curry)
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToDocument, htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.ParentNode (QuerySelector(..), querySelector) as DOM
import DOM.Node.Node (appendChild) as DOM
import DOM.Node.Types (Node, elementToNode) as DOM
import Halogen.VDom as V
import Halogen.VDom.DOM.Prop as P
import Halogen.VDom.Machine as Machine
import Halogen.VDom.Util (refEq)
import Spork.Html (Html)
import Spork.Html.Thunk (buildThunk)

newtype Batch f a = Batch (Compose Array f a)

batch ∷ ∀ f a. Array (f a) → Batch f a
batch as = Batch (Compose as)

unBatch ∷ ∀ f a. Batch f a → Array (f a)
unBatch (Batch (Compose as)) = as

derive newtype instance functorBatch ∷ Functor f ⇒ Functor (Batch f)
derive newtype instance applyBatch ∷ Apply f ⇒ Apply (Batch f)
derive newtype instance applicativeBatch ∷ Applicative f ⇒ Applicative (Batch f)

instance semigroupBatch ∷ Semigroup (Batch f a) where
  append (Batch (Compose as)) (Batch (Compose bs)) = Batch (Compose (as <> bs))

instance monoidBatch ∷ Monoid (Batch f a) where
  mempty = Batch (Compose [])

exec ∷ ∀ f a. f a → Batch f a
exec = batch <<< pure

subscribe ∷ ∀ f a. f a → Batch f a
subscribe = batch <<< pure

purely ∷ ∀ f s i. s → Transition f s i
purely = { model: _, effects: mempty }

type AppEffects eff =
  ( dom ∷ DOM
  , ref ∷ REF
  , exception ∷ EXCEPTION
  | eff
  )

type Transition m s i =
  { model ∷ s
  , effects ∷ Batch m i
  }

type App m q s i =
  { render ∷ s → Html i
  , update ∷ s → i → Transition m s i
  , subs ∷ s → Batch q i
  , init ∷ Transition m s i
  }

type PureApp s i = App (Const Void) (Const Void) s i

type BasicApp m s i = App m (Const Void) s i

type Push eff i = i → Eff eff Unit

newtype Interpreter eff m q i = Interpreter (Push eff i → Coproduct m q i → Eff eff Unit)

newtype Step m i = Step (i → m (Loop m i))

data Loop m i = Loop (i → m (Loop m i)) (m (Step m i))

type EventQueue m i = (i → m Unit) → m (Step m i)

runEventQueue
  ∷ ∀ eff i
  . EventQueue (Eff (ref ∷ REF | eff)) i
  → Eff (ref ∷ REF | eff) (i → Eff (ref ∷ REF | eff) Unit)
runEventQueue proc = do
  queue   ← newRef Nothing
  machine ← newRef Nothing

  let
    push i = do
      q ← readRef queue
      case q of
        Nothing → do
          writeRef queue (Just [i])
          start
        Just is →
          writeRef queue (Just (Array.snoc is i))

    start = do
      step ← readRef machine
      for_ step (loop <<< startLoop)

    startLoop (Step fn) =
      Loop fn (pure (Step fn))

    loop = MR.tailRecM \(Loop next done) → do
      q ← readRef queue
      case q >>= Array.uncons of
        Just { head, tail } → do
          writeRef queue (Just tail)
          MR.Loop <$> next head
        Nothing → do
          step ← done
          q' ← readRef queue
          readRef queue >>= maybe true Array.null >>> if _
            then do
              writeRef machine (Just step)
              writeRef queue Nothing
              pure (MR.Done unit)
            else
              pure (MR.Loop (startLoop step))

  step ← proc push
  writeRef machine (Just step)
  start $> push

run
  ∷ ∀ eff m q s i
  . App m q s i
  → (Interpreter (AppEffects eff) m q i)
  → DOM.Node
  → Eff (AppEffects eff) (i → Eff (AppEffects eff) Unit)
run app (Interpreter interpret) el = runEventQueue \push →
  let
    tick doRender vmach model input = do
      let
        res = app.update model input
        doRender' = doRender || not (runFn2 refEq model res.model)
        render' = if doRender'
          then render vmach res.model
          else pure (Step (tick false vmach res.model))
      runInterpreter (unBatch res.effects) (unBatch (app.subs res.model))
      pure (Loop (tick doRender' vmach res.model) render')

    render vmach model = do
      vmach' ← Machine.step vmach (unwrap (app.render model))
      pure (Step (tick false vmach' model))

    runInterpreter effs subs =
      traverse_ (interpret push) (map left effs <> map right subs)

  in do
    document ←
      DOM.window
        >>= DOM.document
        >>> map DOM.htmlDocumentToDocument
    let
      spec = V.VDomSpec
        { document
        , buildWidget: buildThunk unwrap
        , buildAttributes: P.buildProp push
        }
    vmach ← V.buildVDom spec (unwrap (app.render app.init.model))
    _ ← DOM.appendChild (Machine.extract vmach) el
    runInterpreter (unBatch app.init.effects) (unBatch (app.subs app.init.model))
    pure (Step (tick false vmach app.init.model))

runWithSelector
  ∷ ∀ eff m q s i
  . App m q s i
  → (Interpreter (AppEffects eff) m q i)
  → String
  → Eff (AppEffects eff) (i → Eff (AppEffects eff) Unit)
runWithSelector app interpret sel = do
  win ← DOM.window
  doc ← DOM.document win
  bod ← DOM.querySelector (DOM.QuerySelector sel) (DOM.htmlDocumentToParentNode doc)
  case bod of
    Nothing → throwException (error ("Element does not exist: " <> sel))
    Just el → run app interpret (DOM.elementToNode el)

type InterpreterSpec eff m q s i =
  { spawnEffect ∷ Push eff i → m i → s → Eff eff s
  , spawnSub ∷ Push eff i → q i → s → Eff eff s
  , flush ∷ s → Eff eff s
  , init ∷ Eff eff s
  }

makeInterpreter
  ∷ ∀ eff m q s i
  . InterpreterSpec (ref ∷ REF | eff) m q s i
  → Eff (ref ∷ REF | eff) (Interpreter (ref ∷ REF | eff) m q i)
makeInterpreter spec = Interpreter <<< curry <$> runEventQueue \_ →
  let
    tick state (Tuple push (Coproduct input)) = do
      state' ← case input of
        Left eff  → spec.spawnEffect push eff state
        Right sub → spec.spawnSub push sub state
      pure (Loop (tick state') (flush state'))

    flush state = do
      state' ← spec.flush state
      pure (Step (tick state'))

  in do
    initialState ← spec.init
    pure (Step (tick initialState))

basicInterpreter
  ∷ ∀ eff m i
  . (Push eff i → m i → Eff eff Unit)
  → InterpreterSpec eff m (Const Void) Unit i
basicInterpreter spawn =
  { spawnEffect: \push eff _ → spawn push eff
  , spawnSub: \_ v _ → absurd (unwrap v)
  , flush: pure
  , init: pure unit
  }

pureInterpreter ∷ ∀ eff i. Interpreter eff (Const Void) (Const Void) i
pureInterpreter = Interpreter \_ → coproduct (absurd <<< unwrap) (absurd <<< unwrap)

basicEff ∷ ∀ eff i. InterpreterSpec eff (Eff eff) (Const Void) Unit i
basicEff = toBasicEff id

toBasicEff ∷ ∀ eff f i. (f ~> Eff eff) → InterpreterSpec eff f (Const Void) Unit i
toBasicEff nat = basicInterpreter \push a → push =<< nat a

basicAff ∷ ∀ eff i. (Error → Eff eff Unit) → InterpreterSpec eff (Aff eff) (Const Void) Unit i
basicAff = toBasicAff id

toBasicAff ∷ ∀ eff f i. (f ~> Aff eff) → (Error → Eff eff Unit) → InterpreterSpec eff f (Const Void) Unit i
toBasicAff nat = basicInterpreter <<< \a b c → void (runAff a b (nat c))
