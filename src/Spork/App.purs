module Spork.App
  ( App
  , AppEffects
  , PureApp
  , BasicApp
  , make
  , makeWithSelector
  , module Spork.Batch
  , module Spork.Transition
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
import Data.Functor.Coproduct (Coproduct, left, right)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Newtype (unwrap)
import Data.StrMap as SM
import Halogen.VDom as V
import Halogen.VDom.DOM.Prop as P
import Halogen.VDom.Machine as Machine
import Spork.Batch (Batch, batch, unBatch, lift)
import Spork.EventQueue (EventQueue, Loop(..))
import Spork.EventQueue as EventQueue
import Spork.Html (Html)
import Spork.Html.Thunk (Thunk, buildThunk)
import Spork.Interpreter (Interpreter(..))
import Spork.Transition (purely, Transition)
import Unsafe.Reference (unsafeRefEq)

type AppEffects eff =
  ( dom ∷ DOM
  , ref ∷ REF
  , exception ∷ EXCEPTION
  | eff
  )

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

data AppAction m q s i
  = Restore s
  | Action i
  | Interpret (Coproduct m q i)

type AppState eff m q s i =
  { model ∷ s
  , needsRender ∷ Boolean
  , interpret ∷ Loop (Eff eff) (Coproduct m q i)
  , vdom ∷ Machine.Step (Eff eff) (V.VDom (Array (P.Prop i)) (Thunk Html i)) DOM.Node
  }

makeAppQueue
  ∷ ∀ eff m q s i
  . (AppChange s i → Eff (AppEffects eff) Unit)
  → Interpreter (Eff (AppEffects eff)) (Coproduct m q) i
  → App m q s i
  → DOM.Node
  → EventQueue (Eff (AppEffects eff)) (AppAction m q s i) (AppAction m q s i)
makeAppQueue onChange (Interpreter interpreter) app el = EventQueue.withAccum \self → do
  let
    pushAction = self.push <<< Action
    pushEffect = self.push <<< Interpret <<< left
    pushSub = self.push <<< Interpret <<< right

    queueInterpret ∷ Batch m i → Batch q i → Eff (AppEffects eff) Unit
    queueInterpret effs subs = do
      traverse_ pushEffect (unBatch effs)
      traverse_ pushSub (unBatch subs)

    update
      ∷ AppState (AppEffects eff) m q s i
      → AppAction m q s i
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
        queueInterpret mempty (app.subs nextModel)
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
          Loop _ f → f unit
      pure
        { model: state.model
        , vdom: nextVDom
        , interpret: nextInterpret
        , needsRender: false
        }

  document ←
    DOM.window
      >>= DOM.document
      >>> map DOM.htmlDocumentToDocument
  let
    vdomSpec = V.VDomSpec
      { document
      , buildWidget: buildThunk unwrap
      , buildAttributes: P.buildProp (\a → pushAction a *> self.run)
      }
  vdom ← V.buildVDom vdomSpec (unwrap (app.render app.init.model))
  void $ DOM.appendChild (Machine.extract vdom) el
  interpret ← interpreter (self { push = self.push <<< Action })
  queueInterpret app.init.effects (app.subs app.init.model)
  let
    init =
      { model: app.init.model
      , needsRender: false
      , interpret
      , vdom
      }
  pure { init, update, commit }

make
  ∷ ∀ eff m q s i
  . Interpreter (Eff (AppEffects eff)) (Coproduct m q) i
  → App m q s i
  → DOM.Node
  → Eff (AppEffects eff) (AppInstance (AppEffects eff) s i)
make interpreter app el = do
  subsRef ← newRef { fresh: 0, cbs: SM.empty }
  stateRef ← newRef app.init.model
  let
    handleChange ∷ AppChange s i → Eff (AppEffects eff) Unit
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

    remove ∷ String → Eff (AppEffects eff) Unit
    remove key =
      modifyRef subsRef \subs → subs
        { cbs = SM.delete key subs.cbs
        }

  { push, run } ←
    EventQueue.fix $ makeAppQueue handleChange interpreter app el

  pure
    { push: push <<< Action
    , snapshot: readRef stateRef
    , restore: push <<< Restore
    , subscribe: subscribe'
    , run
    }

makeWithSelector
  ∷ ∀ eff m q s i
  . Interpreter (Eff (AppEffects eff)) (Coproduct m q) i
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
