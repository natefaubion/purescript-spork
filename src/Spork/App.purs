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
  , run
  , runWithSelector
  , module Exports
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, throwException, error)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToDocument, htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.Node (appendChild) as DOM
import DOM.Node.ParentNode (QuerySelector(..), querySelector) as DOM
import DOM.Node.Types (Node, elementToNode) as DOM
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Coproduct (Coproduct, left, right)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (unwrap)
import Halogen.VDom as V
import Halogen.VDom.DOM.Prop as P
import Halogen.VDom.Machine as Machine
import Spork.EventQueue (EventQueue, Loop(..), looped, makeEventQueue, runEventQueue)
import Spork.EventQueue (Push) as Exports
import Spork.Html (Html)
import Spork.Html.Thunk (buildThunk)
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

makeApp
  ∷ ∀ eff m q s i
  . Functor m
  ⇒ Functor q
  ⇒ (Interpreter (AppEffects eff) m q)
  → App m q s i
  → DOM.Node
  → EventQueue (Eff (AppEffects eff)) (Either (Coproduct m q (Eff (AppEffects eff) Unit)) i)
makeApp (Interpreter interpreter) app el = makeEventQueue \push →
  let
    pushInput = push <<< Right
    pushEffect = push <<< Left <<< left
    pushSub = push <<< Left <<< right

    queueInterp effs subs = do
      traverse_ (pushEffect <<< map pushInput) $ unBatch effs
      traverse_ (pushSub <<< map pushInput) $ unBatch subs

    init = do
      document ←
        DOM.window
          >>= DOM.document
          >>> map DOM.htmlDocumentToDocument
      let
        vdomSpec = V.VDomSpec
          { document
          , buildWidget: buildThunk unwrap
          , buildAttributes: P.buildProp pushInput
          }
      vdom ← V.buildVDom vdomSpec (unwrap (app.render app.init.model))
      void $ DOM.appendChild (Machine.extract vdom) el
      interpret ← looped <$> interpreter (push <<< Left)
      queueInterp app.init.effects (app.subs app.init.model)
      pure
        { model: app.init.model
        , needsRender: false
        , interpret
        , vdom
        }

    tick state@{ interpret: Loop k _ } = case _ of
      Left i → do
        nextInterpret ← k i
        pure $ state { interpret = nextInterpret }
      Right i → do
        let
          next = app.update state.model i
          needsRender = state.needsRender || not (unsafeRefEq state.model next.model)
          nextState = state { model = next.model, needsRender = needsRender }
        queueInterp next.effects (app.subs next.model)
        pure nextState

    flush state = do
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
    { init, tick, flush }

run
  ∷ ∀ eff m q s i
  . Functor m
  ⇒ Functor q
  ⇒ (Interpreter (AppEffects eff) m q)
  → App m q s i
  → DOM.Node
  → Eff (AppEffects eff) (i → Eff (AppEffects eff) Unit)
run interpreter app = map (_ <<< Right) <<< runEventQueue <<< makeApp interpreter app

runWithSelector
  ∷ ∀ eff m q s i
  . Functor m
  ⇒ Functor q
  ⇒ (Interpreter (AppEffects eff) m q)
  → App m q s i
  → String
  → Eff (AppEffects eff) (i → Eff (AppEffects eff) Unit)
runWithSelector interpret app sel = do
  win ← DOM.window
  doc ← DOM.document win
  bod ← DOM.querySelector (DOM.QuerySelector sel) (DOM.htmlDocumentToParentNode doc)
  case bod of
    Nothing → throwException (error ("Element does not exist: " <> sel))
    Just el → run interpret app  (DOM.elementToNode el)
