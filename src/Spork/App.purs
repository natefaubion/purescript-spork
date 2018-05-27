module Spork.App
  ( App
  , AppInstance
  , AppChange
  , BasicApp
  , make
  , makeWithSelector
  , module Spork.Batch
  , module Spork.Transition
  ) where

import Prelude

import Data.Const (Const)
import Data.Foldable (for_)
import Data.Functor.Coproduct (Coproduct, left, right)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect (Effect, foreachE)
import Effect.Exception (throwException, error)
import Effect.Ref as Ref
import Effect.Uncurried as EFn
import Foreign.Object as FO
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
import Web.DOM.Element (toNode) as DOMElement
import Web.DOM.Node (Node, appendChild) as DOM
import Web.DOM.ParentNode (QuerySelector(..), querySelector) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toDocument, toParentNode) as HTMLDocument
import Web.HTML.Window (document) as DOM

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
type AppInstance model action =
  { push ∷ action → Effect Unit
  , run ∷ Effect Unit
  , snapshot ∷ Effect model
  , restore ∷ model → Effect Unit
  , subscribe ∷ (AppChange model action → Effect Unit) → Effect (Effect Unit)
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

type AppState m q s i =
  { model ∷ s
  , status ∷ RenderStatus
  , interpret ∷ Loop Effect (Coproduct m q i)
  , vdom ∷ Machine.Step (V.VDom (Array (P.Prop i)) (Thunk Html i)) DOM.Node
  }

makeAppQueue
  ∷ ∀ m q s i
  . (AppChange s i → Effect Unit)
  → Interpreter Effect (Coproduct m q) i
  → App m q s i
  → DOM.Node
  → EventQueue Effect (AppAction m q s i) (AppAction m q s i)
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
      ∷ Loop Effect (Coproduct m q i)
      → Array (q i)
      → Effect (Loop Effect (Coproduct m q i))
    runSubs interpret subs = do
      ref ← Ref.new interpret
      foreachE subs \sub -> do
        Loop k _ ← Ref.read ref
        next ← k (right sub)
        Ref.write next ref
      Ref.read ref

    update
      ∷ AppState m q s i
      → AppAction m q s i
      → Effect (AppState m q s i)
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
        vdom ← EFn.runEffectFn1 (Machine.step state.vdom) (unwrap (app.render state.model))
        pure $ state { vdom = vdom, status = Flushed }

    commit
      ∷ AppState m q s i
      → Effect (AppState m q s i)
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
      >>> map HTMLDocument.toDocument
  let
    vdomSpec = V.VDomSpec
      { document
      , buildWidget: buildThunk unwrap
      , buildAttributes: P.buildProp (\a → pushAction a *> self.run)
      }
  vdom ← EFn.runEffectFn1 (V.buildVDom vdomSpec) (unwrap (app.render app.init.model))
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
  ∷ ∀ effects subs model action
  . Interpreter Effect (Coproduct effects subs) action
  → App effects subs model action
  → DOM.Node
  → Effect (AppInstance model action)
make interpreter app el = do
  subsRef ← Ref.new { fresh: 0, cbs: FO.empty }
  stateRef ← Ref.new app.init.model
  let
    handleChange ∷ AppChange model action → Effect Unit
    handleChange appChange = do
      Ref.write appChange.new stateRef
      subs ← Ref.read subsRef
      for_ subs.cbs (_ $ appChange)

    subscribe'
      ∷ (AppChange model action → Effect Unit)
      → (Effect (Effect Unit))
    subscribe' cb = do
      subs ← Ref.read subsRef
      let key = show subs.fresh
      subsRef # Ref.write
        { fresh: subs.fresh + 1
        , cbs: FO.insert key cb subs.cbs
        }
      pure (remove key)

    remove ∷ String → Effect Unit
    remove key =
      subsRef # Ref.modify_ \subs → subs
        { cbs = FO.delete key subs.cbs
        }

  { push, run } ←
    EventQueue.fix $ makeAppQueue handleChange interpreter app el

  pure
    { push: push <<< Action
    , snapshot: Ref.read stateRef
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
  ∷ ∀ effects subs model action
  . Interpreter Effect (Coproduct effects subs) action
  → App effects subs model action
  → String
  → Effect (AppInstance model action)
makeWithSelector interpret app sel = do
  mbEl ←
    DOM.window
      >>= DOM.document
      >>> map HTMLDocument.toParentNode
      >>= DOM.querySelector (DOM.QuerySelector sel)
  case mbEl of
    Nothing → throwException (error ("Element does not exist: " <> sel))
    Just el → make interpret app (DOMElement.toNode el)
