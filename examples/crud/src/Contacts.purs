module Crud.Contacts where

import Prelude

import Crud.Effect (Effect(..))
import Crud.Route (CrudRoute(..), Route(..), link)
import DOM.Classy.Event (toEvent)
import DOM.Event.Types (Event)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Spork.App as App
import Spork.Html as H
import Spork.Html.Elements.Keyed as HK

data CrudView a
  = ViewIndex
  | ViewEdit a
  | ViewRead a
  | ViewDelete a
  | ViewNotFound

type Model =
  { fresh ∷ Int
  , contacts ∷ Array Contact
  , view ∷ CrudView Contact
  }

type Contact =
  { id ∷ Int
  , name ∷ String
  , email ∷ String
  }

initialModel ∷ Model
initialModel =
  { fresh: 0
  , contacts: []
  , view: ViewIndex
  }

initialContact ∷ Contact
initialContact =
  { id: -1
  , name: ""
  , email: ""
  }

data Action
  = HandleRoute (CrudRoute Int)
  | HandleEdit Contact
  | HandleSubmit Contact Event
  | HandleDelete Contact Boolean
  | Confirm Contact Event
  | Noop

withContact ∷ (Contact → Model → Model) → Int → Model → Model
withContact k contactId model =
  case Array.find (eq contactId <<< _.id) model.contacts of
    Just contact → k contact model
    Nothing      → model { view = ViewNotFound }

viewContact ∷ Int → Model → Model
viewContact = withContact \contact model →
  model { view = ViewRead contact }

editContact ∷ Int → Model → Model
editContact = withContact \contact model →
  model { view = ViewEdit contact }

isNewContact ∷ Contact → Boolean
isNewContact = eq (-1) <<< _.id

update ∷ Model → Action → App.Transition Effect Model Action
update model = case _ of
  HandleRoute CrudIndex →
    App.purely $ model { view = ViewIndex }

  HandleRoute CrudCreate →
    App.purely $ model { view = ViewEdit initialContact }

  HandleRoute (CrudUpdate contactId) →
    App.purely $ editContact contactId model

  HandleRoute (CrudRead contactId) →
    App.purely $ viewContact contactId model

  HandleEdit contact →
    App.purely $ model { view = ViewEdit contact }

  HandleSubmit contact event | isNewContact contact →
    let
      newContact =
        contact { id = model.fresh }

      newModel =
        model
          { fresh = model.fresh + 1
          , contacts = Array.cons newContact model.contacts
          }

      effects =
        App.batch
          [ PreventDefault event Noop
          , Navigate (Contacts CrudIndex) Noop
          ]
    in
      { model: newModel
      , effects
      }

  HandleSubmit contact event →
    let
      newContacts =
        model.contacts
          # map (\c → if c.id == contact.id then contact else c)

      newModel =
        model { contacts = newContacts }

      effects =
        App.batch
          [ PreventDefault event Noop
          , Navigate (Contacts (CrudRead contact.id)) Noop
          ]
    in
      { model: newModel
      , effects
      }

  HandleDelete contact true →
    let
      newContacts =
        Array.filter (not eq contact.id <<< _.id) model.contacts

      newModel =
        model { contacts = newContacts }

      effects =
        App.lift $ Navigate (Contacts CrudIndex) Noop
    in
      { model: newModel
      , effects
      }

  HandleDelete contact false →
    App.purely $ model { view = ViewRead contact }

  Confirm contact event →
    let
      newModel =
        model { view = ViewDelete contact }

      effects =
        App.lift $ PreventDefault event Noop
    in
      { model: newModel
      , effects
      }

  Noop →
    App.purely model

render ∷ Model → H.Html Action
render model = case model.view of
  ViewIndex →
    H.lazy renderIndex model.contacts

  ViewEdit contact →
    H.lazy renderForm contact

  ViewDelete contact →
    H.lazy renderConfirmation contact

  ViewRead contact →
    H.lazy renderContact contact

  ViewNotFound →
    contactNotFound

contactNotFound ∷ forall a. H.Html a
contactNotFound =
  H.p [] [ H.text "Contact not found." ]

emptyContacts ∷ forall a. H.Html a
emptyContacts =
  H.p [] [ H.text "You have no contacts." ]

renderIndex ∷ Array Contact → H.Html Action
renderIndex contacts =
  H.div
    [ H.classes [ "contacts" ] ]
    [ H.div
        [ H.classes [ "toolbar" ] ]
        [ link (Contacts CrudCreate) [] [ H.text "New" ] ]
    , if Array.null contacts
        then emptyContacts
        else renderList contacts
    ]

renderList ∷ Array Contact → H.Html Action
renderList contacts =
  HK.ul
    [ H.classes [ "contact-list" ] ]
    (renderItem <$> contacts)
  where
  renderItem contact =
    Tuple (show contact.id) $ H.lazy renderLabel contact

renderLabel ∷ Contact → H.Html Action
renderLabel contact =
  H.li []
    [ link (Contacts (CrudRead contact.id)) []
        [ H.text contact.name ]
    ]

renderContact ∷ Contact → H.Html Action
renderContact contact =
  H.div
    [ H.classes [ "contact" ] ]
    [ H.div
        [ H.classes [ "toolbar" ] ]
        [ link (Contacts (CrudUpdate contact.id)) [] [ H.text "Edit" ]
        , H.a
            [ H.href "#"
            , H.onClick $ H.always $ Confirm contact <<< toEvent
            ]
            [ H.text "Delete" ]
        ]
    , H.dl []
        [ H.dt [] [ H.text "Name" ]
        , H.dd [] [ H.text contact.name ]
        , H.dt [] [ H.text "Email" ]
        , H.dd [] [ H.text contact.email ]
        ]
    ]

renderForm ∷ Contact → H.Html Action
renderForm contact =
  H.form
    [ H.classes [ "form" ]
    , H.onSubmit $ H.always $ HandleSubmit contact
    ]
    [ formRow "Name" $ H.input
        [ H.type_ H.InputText
        , H.value contact.name
        , H.onValueInput $ H.always $ HandleEdit <<< contact { name = _ }
        ]
    , formRow "Email" $ H.input
        [ H.type_ H.InputText
        , H.value contact.email
        , H.onValueInput $ H.always $ HandleEdit <<< contact { email = _ }
        ]
    , H.button
        [ H.type_ H.ButtonSubmit ]
        [ H.text "Save" ]
    ]

renderConfirmation ∷ Contact → H.Html Action
renderConfirmation contact =
  H.div
    [ H.classes [ "confirmation" ] ]
    [ H.p []
        [ H.text "Are you sure you want to delete this contact?" ]
    , H.p []
        [ H.button
            [ H.type_ H.ButtonButton
            , H.onClick $ H.always_ $ HandleDelete contact false
            ]
            [ H.text "No" ]
        , H.button
            [ H.type_ H.ButtonButton
            , H.onClick $ H.always_ $ HandleDelete contact true
            ]
            [ H.text "Yes, Delete" ]
        ]
    ]

formRow ∷ forall a. String → H.Html a → H.Html a
formRow label input =
  H.div
    [ H.classes [ "form-row" ] ]
    [ H.label
        [ H.classes [ "form-row-label" ] ]
        [ H.text label ]
    , H.div
        [ H.classes [ "form-row-input" ] ]
        [ input ]
    ]
