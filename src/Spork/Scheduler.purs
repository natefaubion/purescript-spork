module Spork.Scheduler
  ( makeImmediate
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, modifyRef, newRef, readRef, writeRef)
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToDocument) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.Document (createTextNode) as DOM
import DOM.Node.MutationObserver (mutationObserver, observe) as DOM
import DOM.Node.Node (setNodeValue) as DOM
import DOM.Node.Types (textToNode) as DOM
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)

makeImmediate
  ∷ ∀ eff
  . Eff (ref ∷ REF, dom ∷ DOM | eff) Unit
  → Eff (ref ∷ REF, dom ∷ DOM | eff) (Eff (ref ∷ REF, dom ∷ DOM | eff) Unit)
makeImmediate run = do
  document ←
    DOM.window
      >>= DOM.document
      >>> map DOM.htmlDocumentToDocument
  nextTick ← newRef (Right 0)
  obsvNode ← DOM.textToNode <$> DOM.createTextNode "" document
  observer ← DOM.mutationObserver \_ _ → do
    modifyRef nextTick $ either (Right <<< add 1) Right
    run
  DOM.observe obsvNode { characterData: true } observer
  pure do
    readRef nextTick >>= traverse_ \tick → do
      writeRef nextTick $ Left (tick + 1)
      DOM.setNodeValue (show tick) obsvNode
