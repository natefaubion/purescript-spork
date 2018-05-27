module Spork.Scheduler
  ( makeImmediate
  ) where

import Prelude

import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Ref as Ref
import Web.DOM.Document (createTextNode) as DOM
import Web.DOM.MutationObserver (mutationObserver, observe) as DOM
import Web.DOM.Node (setNodeValue) as DOM
import Web.DOM.Text (toNode) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toDocument) as DOM
import Web.HTML.Window (document) as DOM

makeImmediate ∷ Effect Unit → Effect (Effect Unit)
makeImmediate run = do
  document ←
    DOM.window
      >>= DOM.document
      >>> map DOM.toDocument
  nextTick ← Ref.new (Right 0)
  obsvNode ← DOM.toNode <$> DOM.createTextNode "" document
  observer ← DOM.mutationObserver \_ _ → do
    Ref.modify_ (either (Right <<< add 1) Right) nextTick
    run
  DOM.observe obsvNode { characterData: true } observer
  pure do
    Ref.read nextTick >>= traverse_ \tick → do
      Ref.write (Left (tick + 1)) nextTick
      DOM.setNodeValue (show tick) obsvNode
