module Spork.App
  ( App
  , AppEffects
  , AppInstance
  , AppChange
  , BasicApp
  , make
  , makeWithSelector
  , module Spork.Batch
  , module Spork.Transition
  ) where

import Prelude

import Control.Monad.Eff (Eff, foreachE)
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
import Data.Foldable (for_)
import Data.Functor.Coproduct (Coproduct, left, right)
import Data.Maybe (Maybe(..))
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
import Spork.Scheduler (makeImmediate)
import Spork.Transition (purely, Transition)
import Unsafe.Reference (unsafeRefEq)

type AppEffects eff =
  ( dom ∷ DOM
  , ref ∷ REF
  , exception ∷ EXCEPTION
  | eff
  )

-- | A specification for a Spork app:
-- |    * `render` - Renders a model to `Html` which yields actions via DOM events.
-- |    * `update` - Takes the current model and, with a new action, transitions to a new model while optionally running effects.
-- |    * `subs` - Determines the set of active subscriptions based on the model.
-- |    * `init` - Initial model and effects to kickstart the application.
type App effects subs model action =
  { render ∷ model → Html action
  , update ∷ model → action → Transition effects model action
  , subs ∷ model → Batch subs action
  , init ∷ Transition effects model action
  }

-- | A type synonym for Apps which don't have subs.
type BasicApp effects model action = App effects (Const Void) model action

-- | The interface for communicating with a running App.
-- |    * `push` - Buffers an action to be run on the next tick.
-- |    * `run` - Initiates a tick of the App, flushing and applying all queued actions.
-- |    * `snapshot` - Yields the current model of the App.
-- |    * `restore` - Replaces the current model of the App.
-- |    * `subscribe` - Listens to App changes (model and actions).
type AppInstance eff model action =
  { push ∷ action → Eff eff Unit
  , run ∷ Eff eff Unit
  , snapshot ∷ Eff eff model
  , restore ∷ model → Eff eff Unit
  , subscribe ∷ (AppChange model action → Eff eff Unit) → Eff eff (Eff eff Unit)
  }

type AppChange model action =
  { old ∷ model
  , action ∷ action
  , new ∷ model
  }

data AppAction m q s i
  = Restore s
  | Action i
  | Interpret (Coproduct m q i)
  | Render

data RenderStatus
  = NoChange
  | Pending
  | Flushed

derive instance eqRenderStatus ∷ Eq RenderStatus

type AppState eff m q s i =
  { model ∷ s
  , status ∷ RenderStatus
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
  schedule ← makeImmediate (self.push Render *> self.run)
  let
    pushAction = self.push <<< Action
    pushEffect = self.push <<< Interpret <<< left

    nextStatus ∷ s → s → RenderStatus → RenderStatus
    nextStatus prevModel nextModel = case _ of
      NoChange
        | unsafeRefEq prevModel nextModel → NoChange
        | otherwise → Pending
      Flushed → NoChange
      Pending → Pending

    runSubs
      ∷ Loop (Eff (AppEffects eff)) (Coproduct m q i)
      → Array (q i)
      → Eff (AppEffects eff) (Loop (Eff (AppEffects eff)) (Coproduct m q i))
    runSubs interpret subs = do
      ref ← newRef interpret
      foreachE subs \sub -> do
        Loop k _ ← readRef ref
        next ← k (right sub)
        writeRef ref next
      readRef ref

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
          status = nextStatus state.model next.model state.status
          nextState = state { model = next.model, status = status }
          appChange = { old: state.model, action: i, new: next.model }
        onChange appChange
        foreachE (unBatch next.effects) pushEffect
        pure nextState
      Restore nextModel → do
        let
          status = nextStatus state.model nextModel state.status
          nextState = state { model = nextModel, status = status }
        pure nextState
      Render → do
        vdom ← Machine.step state.vdom (unwrap (app.render state.model))
        pure $ state { vdom = vdom, status = Flushed }

    commit
      ∷ AppState (AppEffects eff) m q s i
      → Eff (AppEffects eff) (AppState (AppEffects eff) m q s i)
    commit state = case state.status of
      Flushed →
        pure $ state { status = NoChange }
      status → do
        when (status == Pending) schedule
        tickInterpret ← runSubs state.interpret (unBatch (app.subs state.model))
        nextInterpret ← case tickInterpret of Loop _ f → f unit
        pure $ state { interpret = nextInterpret, status = NoChange }

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
  foreachE (unBatch app.init.effects) pushEffect
  let
    init =
      { model: app.init.model
      , status: NoChange
      , interpret
      , vdom
      }
  pure { init, update, commit }

-- | Builds a running App given an `Interpreter` and a parent DOM Node.
-- |
-- | ```purescript
-- | example domNode = do
-- |   inst <- App.make (basicAff `merge` never) app domNode
-- |   _    <- inst.subscribe \_ -> log "Got a change!"
-- |   inst.run
-- | ```
-- |
-- | The returned `AppInstance` has yet to run any initial effects. You may
-- | use the opportunity to setup change handlers. Invoke `inst.run` when
-- | ready to run initial effects.
make
  ∷ ∀ eff effects subs model action
  . Interpreter (Eff (AppEffects eff)) (Coproduct effects subs) action
  → App effects subs model action
  → DOM.Node
  → Eff (AppEffects eff) (AppInstance (AppEffects eff) model action)
make interpreter app el = do
  subsRef ← newRef { fresh: 0, cbs: SM.empty }
  stateRef ← newRef app.init.model
  let
    handleChange ∷ AppChange model action → Eff (AppEffects eff) Unit
    handleChange appChange = do
      writeRef stateRef appChange.new
      subs ← readRef subsRef
      for_ subs.cbs (_ $ appChange)

    subscribe'
      ∷ (AppChange model action → Eff (AppEffects eff) Unit)
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

-- | Builds a running App given an `Interpreter` and a DOM selector.
-- |
-- | ```purescript
-- | main = do
-- |   inst <- App.makeWithSelector (basicAff `merge` never) app "#app"
-- |   _    <- inst.subscribe \_ -> log "Got a change!"
-- |   inst.run
-- | ```
-- |
-- | The returned `AppInstance` has yet to run any initial effects. You may
-- | use the opportunity to setup change handlers. Invoke `inst.run` when
-- | ready to run initial effects.
makeWithSelector
  ∷ ∀ eff effects subs model action
  . Interpreter (Eff (AppEffects eff)) (Coproduct effects subs) action
  → App effects subs model action
  → String
  → Eff (AppEffects eff) (AppInstance (AppEffects eff) model action)
makeWithSelector interpret app sel = do
  mbEl ←
    DOM.window
      >>= DOM.document
      >>> map DOM.htmlDocumentToParentNode
      >>= DOM.querySelector (DOM.QuerySelector sel)
  case mbEl of
    Nothing → throwException (error ("Element does not exist: " <> sel))
    Just el → make interpret app (DOM.elementToNode el)
