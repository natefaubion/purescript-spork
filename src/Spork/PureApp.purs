module Spork.PureApp
  ( PureApp
  , make
  , makeWithSelector
  , toApp
  , module Spork.App
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import DOM.Node.Types as DOM
import Data.Const (Const)
import Data.Monoid (mempty)
import Spork.App (App, AppEffects, AppInstance, AppChange)
import Spork.App as App
import Spork.Html (Html)
import Spork.Interpreter (merge, never)

type PureApp model action =
  { render ∷ model → Html action
  , update ∷ model → action → model
  , init ∷ model
  }

-- | Builds a running `PureApp`.
-- |
-- | ```purescript
-- | example domNode = do
-- |   inst <- PureApp.make app domNode
-- |   _    <- inst.subscribe \_ -> log "Got a change!"
-- | ```
make
  ∷ ∀ eff model action
  . PureApp model action
  → DOM.Node
  → Eff (AppEffects eff) (AppInstance (AppEffects eff) model action)
make = App.make (never `merge` never) <<< toApp

-- | Builds a running `PureApp` given a DOM selector.
-- |
-- | ```purescript
-- | main = do
-- |   inst <- PureApp.makeWithSelector app "#app"
-- |   _    <- inst.subscribe \_ -> log "Got a change!"
-- | ```
makeWithSelector
  ∷ ∀ eff model action
  . PureApp model action
  → String
  → Eff (AppEffects eff) (AppInstance (AppEffects eff) model action)
makeWithSelector = App.makeWithSelector (never `merge` never) <<< toApp

-- | Converts a `PureApp` to a regular `App`.
toApp
  ∷ ∀ model action
  . PureApp model action
  → App (Const Void) (Const Void) model action
toApp app =
  { render: app.render
  , update: \model action → App.purely (app.update model action)
  , init: App.purely app.init
  , subs: mempty
  }
