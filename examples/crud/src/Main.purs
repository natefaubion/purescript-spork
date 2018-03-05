module Crud.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Exception (message)
import Control.MonadZero (guard)
import Crud.Contacts as Contacts
import Crud.Effect (Effect, runEffect)
import Crud.Route (CrudRoute(..), Route(..), link, parseRoute)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Routing.Hash as Routing
import Spork.App (BasicApp)
import Spork.App as App
import Spork.Html as H
import Spork.Interpreter (merge, never, throughAff)

type Model =
  { contacts ∷ Contacts.Model
  , view ∷ Maybe View
  }

data View = ViewHome | ViewContacts

derive instance eqView ∷ Eq View

initialModel ∷ Model
initialModel =
  { contacts: Contacts.initialModel
  , view: Just ViewHome
  }

data Action
  = HandleRoute (Maybe Route)
  | ContactsAction Contacts.Action

update ∷ Model → Action → App.Transition Effect Model Action
update model = case _ of
  HandleRoute (Just Home) →
    App.purely $ model { view = Just ViewHome }

  HandleRoute (Just (Contacts crud)) →
    let
      trans =
        Contacts.update model.contacts (Contacts.HandleRoute crud)

      newModel =
        { view: Just ViewContacts
        , contacts: trans.model
        }
    in
      { model: newModel
      , effects: ContactsAction <$> trans.effects
      }

  HandleRoute Nothing →
    App.purely $ model { view = Nothing }

  ContactsAction action →
    let
      newUpdate =
        Contacts.update model.contacts action

      newModel =
        model { contacts = newUpdate.model }
    in
      { model: newModel
      , effects: ContactsAction <$> newUpdate.effects
      }

render ∷ Model → H.Html Action
render model =
  H.div
    [ H.classes [ "container" ] ]
    [ H.div
        [ H.classes [ "header" ] ]
        [ H.h1 [] [ H.text "CRUD Demo" ]
        , H.lazy renderTabs model.view
        ]
    , H.div
        [ H.classes [ "page" ] ]
        [ renderPage model ]
    ]

renderTabs ∷ Maybe View → H.Html Action
renderTabs currentView =
  H.ul
    [ H.classes [ "tabs" ] ]
    [ tabItem ViewHome Home
    , tabItem ViewContacts (Contacts CrudIndex)
    ]
  where
  tabItem view route =
    H.li
      [ H.classes ("selected" <$ guard (Just view == currentView)) ]
      [ link route [] [ H.text (printView view) ] ]

printView ∷ View → String
printView = case _ of
  ViewHome → "Home"
  ViewContacts → "Contacts"

renderPage ∷ Model → H.Html Action
renderPage model = case model.view of
  Just ViewHome →
    homeContent

  Just ViewContacts →
    ContactsAction <$> Contacts.render model.contacts

  Nothing →
    pageNotFound

homeContent ∷ forall a. H.Html a
homeContent =
  H.div []
    [ H.p []
        [ H.text """
            This app demonstrates embedding a sub-page with routing and effects.
          """
        ]
    ]

pageNotFound ∷ forall a. H.Html a
pageNotFound =
  H.div []
    [ H.p []
        [ H.text """
            Page not found.
          """
        ]
    ]

app ∷ BasicApp Effect Model Action
app =
  { render
  , update
  , subs: mempty
  , init: App.purely initialModel
  }

main ∷ Eff (App.AppEffects (console ∷ CONSOLE)) Unit
main = do
  let
    handleError err =
      Console.error $ "Error in Main: " <> message err

  inst ←
    App.makeWithSelector
      (throughAff runEffect handleError `merge` never)
      app
      "#app"

  void $ Routing.hashes \_ newHash → do
    inst.push $ HandleRoute $ parseRoute newHash
    inst.run
