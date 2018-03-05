module Crud.Effect
  ( Effect(..)
  , runEffect
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Crud.Route (Route, printRoute)
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import DOM.Event.Types (Event)
import Routing.Hash (setHash)

data Effect a
  = Navigate Route a
  | PreventDefault Event a

derive instance functorEffect ∷ Functor Effect

runEffect ∷ forall eff. Effect ~> Aff (dom :: DOM | eff)
runEffect = case _ of
  Navigate route next → do
    liftEff $ setHash $ printRoute route
    pure next

  PreventDefault event next → do
    liftEff $ preventDefault event
    pure next
