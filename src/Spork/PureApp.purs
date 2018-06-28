module Spork.PureApp
  ( PureApp
  , make
  , makeWithSelector
  , toApp
  , module Spork.App
  ) where

import Prelude

import Effect (Effect)
import Spork.App (App, AppInstance, AppChange)
import Spork.App as App
import Spork.Html (Html)
import Spork.Interpreter (merge, never)
import Web.DOM.Node (Node)

-- | A `PureApp` has no effects or subscriptions.
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
  ∷ ∀ model action
  . PureApp model action
  → Node
  → Effect (AppInstance model action)
make = App.make (never `merge` never) <<< toApp

-- | Builds a running `PureApp` given a DOM selector.
-- |
-- | ```purescript
-- | main = do
-- |   inst <- PureApp.makeWithSelector app "#app"
-- |   _    <- inst.subscribe \_ -> log "Got a change!"
-- | ```
makeWithSelector
  ∷ ∀ model action
  . PureApp model action
  → String
  → Effect (AppInstance model action)
makeWithSelector = App.makeWithSelector (never `merge` never) <<< toApp

-- | Converts a `PureApp` to a regular `App`.
toApp
  ∷ ∀ model action void1 void2
  . PureApp model action
  → App void1 void2 model action
toApp app =
  { render: app.render
  , update: \model action → App.purely (app.update model action)
  , init: App.purely app.init
  , subs: mempty
  }
