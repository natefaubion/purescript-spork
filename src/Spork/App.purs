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
  , make
  , makeWithSelector
  , module Exports
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, throwException, error)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef, modifyRef)
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToDocument, htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.Node (appendChild) as DOM
import DOM.Node.ParentNode (QuerySelector(..), querySelector) as DOM
import DOM.Node.Types (Node, elementToNode) as DOM
import Data.Const (Const)
import Data.Foldable (for_, traverse_)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Coproduct (Coproduct, left, right)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (unwrap)
import Data.StrMap as SM
import Halogen.VDom as V
import Halogen.VDom.DOM.Prop as P
import Halogen.VDom.Machine as Machine
import Spork.EventQueue (EventQueue, Loop(..), fromEventQueueSpec, looped, makeEventQueue)
import Spork.EventQueue (Push) as Exports
import Spork.Html (Html)
import Spork.Html.Thunk (Thunk, buildThunk)
import Spork.Interpreter (Interpreter(..))
import Unsafe.Reference (unsafeRefEq)

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

type AppInstance eff s i =
  { push ∷ i → Eff eff Unit
  , snapshot ∷ Eff eff s
  , restore ∷ s → Eff eff Unit
  , subscribe ∷ (AppChange s i → Eff eff Unit) → Eff eff (Eff eff Unit)
  , run ∷ Eff eff Unit
  }

type AppChange s i =
  { old ∷ s
  , action ∷ i
  , new ∷ s
  }

data AppAction eff m q s i
  = Restore s
  | Action i
  | Interpret (Coproduct m q (Eff eff Unit))

type AppState eff m q s i =
  { model ∷ s
  , needsRender ∷ Boolean
  , interpret ∷ Loop (Eff eff) (Coproduct m q (Eff eff Unit))
  , vdom ∷ Machine.Step (Eff eff) (V.VDom (Array (P.Prop i)) (Thunk Html i)) DOM.Node
  }

makeAppQueue
  ∷ ∀ eff m q s i
  . Functor m
  ⇒ Functor q
  ⇒ (AppChange s i → Eff (AppEffects eff) Unit)
  → Interpreter (Coproduct m q) (Eff (AppEffects eff))
  → App m q s i
  → DOM.Node
  → EventQueue (Eff (AppEffects eff)) (AppAction (AppEffects eff) m q s i)
makeAppQueue onChange (Interpreter interpreter) app el = fromEventQueueSpec \push →
  let
    pushAction = push <<< Action
    pushEffect = push <<< Interpret <<< left
    pushSub = push <<< Interpret <<< right

    queueInterpret
      ∷ Batch m i
      → Batch q i
      → Eff (AppEffects eff) Unit
    queueInterpret effs subs = do
      traverse_ (pushEffect <<< map pushAction) $ unBatch effs
      traverse_ (pushSub <<< map pushAction) $ unBatch subs

    init ∷ Eff (AppEffects eff) (AppState (AppEffects eff) m q s i)
    init = do
      document ←
        DOM.window
          >>= DOM.document
          >>> map DOM.htmlDocumentToDocument
      let
        vdomSpec = V.VDomSpec
          { document
          , buildWidget: buildThunk unwrap
          , buildAttributes: P.buildProp pushAction
          }
      vdom ← V.buildVDom vdomSpec (unwrap (app.render app.init.model))
      void $ DOM.appendChild (Machine.extract vdom) el
      interpret ← looped <$> interpreter (push <<< Interpret)
      queueInterpret app.init.effects (app.subs app.init.model)
      pure
        { model: app.init.model
        , needsRender: false
        , interpret
        , vdom
        }

    update
      ∷ AppState (AppEffects eff) m q s i
      → AppAction (AppEffects eff) m q s i
      → Eff (AppEffects eff) (AppState (AppEffects eff) m q s i)
    update state@{ interpret: Loop k _ } = case _ of
      Interpret m → do
        nextInterpret ← k m
        pure $ state { interpret = nextInterpret }
      Action i → do
        let
          next = app.update state.model i
          needsRender = state.needsRender || not (unsafeRefEq state.model next.model)
          nextState = state { model = next.model, needsRender = needsRender }
          appChange = { old: state.model, action: i, new: next.model }
        onChange appChange
        queueInterpret next.effects (app.subs next.model)
        pure nextState
      Restore nextModel → do
        let
          needsRender = state.needsRender || not (unsafeRefEq state.model nextModel)
          nextState = state { model = nextModel, needsRender = needsRender }
        pure nextState

    commit
      ∷ AppState (AppEffects eff) m q s i
      → Eff (AppEffects eff) (AppState (AppEffects eff) m q s i)
    commit state = do
      nextVDom ←
        if state.needsRender
          then Machine.step state.vdom (unwrap (app.render state.model))
          else pure state.vdom
      nextInterpret ←
        case state.interpret of
          Loop _ f → looped <$> f
      pure
        { model: state.model
        , vdom: nextVDom
        , interpret: nextInterpret
        , needsRender: false
        }
  in
    { init, update, commit }

make
  ∷ ∀ eff m q s i
  . Functor m
  ⇒ Functor q
  ⇒ Interpreter (Coproduct m q) (Eff (AppEffects eff))
  → App m q s i
  → DOM.Node
  → Eff (AppEffects eff) (AppInstance (AppEffects eff) s i)
make interpreter app el = do
  subsRef ← newRef { fresh: 0, cbs: SM.empty }
  stateRef ← newRef app.init.model
  let
    handleChange
      ∷ AppChange s i
      → Eff (AppEffects eff) Unit
    handleChange appChange = do
      writeRef stateRef appChange.new
      subs ← readRef subsRef
      for_ subs.cbs (_ $ appChange)

    subscribe'
      ∷ (AppChange s i → Eff (AppEffects eff) Unit)
      → (Eff (AppEffects eff) (Eff (AppEffects eff) Unit))
    subscribe' cb = do
      subs ← readRef subsRef
      let key = show subs.fresh
      writeRef subsRef
        { fresh: subs.fresh + 1
        , cbs: SM.insert key cb subs.cbs
        }
      pure (remove key)

    remove
      ∷ String
      → Eff (AppEffects eff) Unit
    remove key =
      modifyRef subsRef \subs → subs
        { cbs = SM.delete key subs.cbs
        }

  { push, run } ←
    makeEventQueue $ makeAppQueue handleChange interpreter app el

  pure
    { push: push <<< Action
    , snapshot: readRef stateRef
    , restore: push <<< Restore
    , subscribe: subscribe'
    , run
    }

makeWithSelector
  ∷ ∀ eff m q s i
  . Functor m
  ⇒ Functor q
  ⇒ Interpreter (Coproduct m q) (Eff (AppEffects eff))
  → App m q s i
  → String
  → Eff (AppEffects eff) (AppInstance (AppEffects eff) s i)
makeWithSelector interpret app sel = do
  mbEl ←
    DOM.window
      >>= DOM.document
      >>> map DOM.htmlDocumentToParentNode
      >>= DOM.querySelector (DOM.QuerySelector sel)
  case mbEl of
    Nothing → throwException (error ("Element does not exist: " <> sel))
    Just el → make interpret app (DOM.elementToNode el)
