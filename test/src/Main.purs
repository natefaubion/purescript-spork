module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.MonadZero (guard)
import DOM (DOM)
import DOM.Classy.Element (fromElement) as DOM
import DOM.Event.KeyboardEvent (key) as DOM
import DOM.Event.Types (KeyboardEvent) as DOM
import DOM.HTML (window) as DOM
import DOM.HTML.HTMLElement (focus) as DOM
import DOM.HTML.Window (localStorage) as DOM
import DOM.Node.Types (Element) as DOM
import DOM.WebStorage.Storage (getItem, setItem) as DOM
import Data.Array as Array
import Data.Const (Const)
import Data.Either (hush)
import Data.Foldable as F
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))
import Routing (hashes)
import Simple.JSON (readJSON, writeJSON)
import Spork.App as App
import Spork.Html as H
import Spork.Html.Elements.Keyed as K
import Spork.Interpreter (toBasicEff)

type Model =
  { todos ∷ Array Todo
  , pending ∷ String
  , fresh ∷ Int
  , visibility ∷ Visibility
  }

data Visibility
  = All
  | Active
  | Completed

derive instance eqVisibility ∷ Eq Visibility

instance showVisibility ∷ Show Visibility where
  show = case _ of
    All → "All"
    Active → "Active"
    Completed → "Completed"

type Todo =
  { text ∷ String
  , completed ∷ Boolean
  , editing ∷ Boolean
  , id ∷ Int
  }

initialModel ∷ Model
initialModel =
  { todos: []
  , pending: ""
  , fresh: 0
  , visibility: All
  }

data Action
  = None
  | UpdatePending String
  | AddTodo
  | UpdateTodo Int String
  | ToggleTodo Int Boolean
  | EditingTodo Int Boolean
  | DeleteTodo Int
  | DeleteCompleted
  | ToggleAll Boolean
  | ChangeVisibility Visibility
  | TodoElement H.ElementRef

newTodo ∷ String → Int → Todo
newTodo = { text: _, id: _, completed: false, editing: false }

modifyWhere ∷ forall f a. Functor f ⇒ (a → Boolean) → (a → a) → f a → f a
modifyWhere pred mod = map (\a → if pred a then mod a else a)

toStorage ∷ Model → App.Transition Effect Model Action
toStorage model =
  { model
  , effects: App.exec (WriteStorage model None)
  }

update ∷ Model → Action → App.Transition Effect Model Action
update model = case _ of
  None →
    App.purely model

  UpdatePending pending →
    App.purely $ model { pending = pending }

  AddTodo →
    let
      todos' =
        if model.pending == ""
          then model.todos
          else Array.snoc model.todos (newTodo model.pending model.fresh)
    in
      toStorage $ model
        { pending = ""
        , todos = todos'
        , fresh = model.fresh + 1
        }

  UpdateTodo todo text →
    let
      todos' =
        model.todos
          # modifyWhere (eq todo <<< _.id)
          _ { text = text }
    in
      toStorage $ model { todos = todos' }

  ToggleTodo todo checked →
    let
      todos' =
        model.todos
          # modifyWhere (eq todo <<< _.id)
          _ { completed = checked }
    in
      toStorage $ model { todos = todos' }

  EditingTodo todo editing →
    let
      todos' =
        model.todos
          # modifyWhere (eq todo <<< _.id)
          _ { editing = editing }
    in
      toStorage $ model { todos = todos' }

  DeleteTodo todo →
    let
      todos' =
        model.todos
          # Array.filter (not eq todo <<< _.id)
    in
      toStorage $ model { todos = todos' }

  DeleteCompleted →
    let
      todos' =
        model.todos
          # Array.filter (not _.completed)
    in
      toStorage $ model { todos = todos' }

  ToggleAll checked →
    let
      todos' =
        model.todos
          # map _ { completed = checked }
    in
      toStorage $ model { todos = todos' }

  ChangeVisibility visibility →
    App.purely $  model { visibility = visibility }

  TodoElement ref →
    let
      effects = case ref of
        H.Created el → App.exec (Focus el None)
        H.Removed _  → mempty
    in
      { model, effects }

render ∷ Model → H.Html Action
render model =
  H.div
    [ H.classes [ "todomvc-wrapper" ] ]
    [ H.section
        [ H.classes [ "todoapp" ] ]
        [ H.lazy renderInput model.pending
        , H.lazy2 renderTodos model.visibility model.todos
        , H.lazy2 renderControls model.visibility model.todos
        ]
    , infoFooter
    ]

renderInput ∷ String → H.Html Action
renderInput value =
  H.header
    [ H.classes [ "header" ] ]
    [ H.h1 [] [ H.text "todos" ]
    , H.input
        [ H.classes [ "new-todo" ]
        , H.type_ H.InputText
        , H.value value
        , H.name "newTodo"
        , H.placeholder "What needs to be done?"
        , H.autofocus true
        , H.onValueInput (H.always UpdatePending)
        , onEnter AddTodo
        ]
    ]

renderTodos ∷ Visibility → Array Todo → H.Html Action
renderTodos visibility todos =
  let
    filteredTodos = case visibility of
      All →
        todos
      Active →
        Array.filter (not _.completed) todos
      Completed →
        Array.filter _.completed todos

    allCompleted =
      F.all _.completed todos
  in
    H.section
      [ H.classes [ "main" ]
      , styleHidden (Array.null todos)
      ]
      [ H.input
          [ H.classes [ "toggle-all" ]
          , H.type_ H.InputCheckbox
          , H.name "toggle"
          , H.id_ "toggle-all"
          , H.checked allCompleted
          , H.onChecked (H.always_ (ToggleAll (not allCompleted)))
          ]
      , H.label
          [ H.for "toggle-all" ]
          [ H.text "Mark all as completed" ]
      , K.ul
          [ H.classes [ "todo-list" ] ]
          (map renderKeyedTodo filteredTodos)
      ]

renderKeyedTodo ∷ Todo → Tuple String (H.Html Action)
renderKeyedTodo todo =
  Tuple (show todo.id) (H.lazy renderTodo todo)

renderTodo ∷ Todo → H.Html Action
renderTodo todo =
  H.li
    [ H.classes $ F.fold
        [ "completed" <$ guard todo.completed
        , "editing" <$ guard todo.editing
        ]
    ]
    [ if todo.editing
        then
          H.input
            [ H.classes [ "edit" ]
            , H.type_ H.InputText
            , H.value todo.text
            , H.ref (H.always TodoElement)
            , H.onValueInput (H.always (UpdateTodo todo.id))
            , H.onBlur (H.always_ (EditingTodo todo.id false))
            , onEnter (EditingTodo todo.id false)
            ]
        else
          H.div
            [ H.classes [ "view" ] ]
            [ H.input
                [ H.classes [ "toggle" ]
                , H.type_ H.InputCheckbox
                , H.checked todo.completed
                , H.onChecked (H.always (ToggleTodo todo.id))
                ]
            , H.label
                [ H.onDoubleClick (H.always_ (EditingTodo todo.id true)) ]
                [ H.text todo.text ]
            , H.button
                [ H.classes [ "destroy" ]
                , H.onClick \_ → Just (DeleteTodo todo.id)
                , H.type_ H.ButtonButton
                ]
                []
            ]
    ]

renderControls ∷ Visibility → Array Todo → H.Html Action
renderControls visibility todos =
  let
    lenCompleted =
      todos
        # Array.filter _.completed
        # Array.length

    lenLeft =
      Array.length todos - lenCompleted
  in
    H.footer
      [ H.classes [ "footer" ]
      , H.hidden (Array.null todos)
      ]
      [ H.lazy renderCount lenLeft
      , H.lazy renderFilters visibility
      , H.lazy renderClear lenCompleted
      ]

renderCount ∷ forall i. Int → H.Html i
renderCount len =
  let
    item =
      if len == 1
        then " item"
        else " items"
  in
    H.span
      [ H.classes [ "todo-count" ] ]
      [ H.strong [] [ H.text (show len) ]
      , H.text item
      ]

renderFilters ∷ Visibility → H.Html Action
renderFilters visibility =
  H.ul
    [ H.classes [ "filters" ] ]
    [ visibilityLink All visibility
    , visibilityLink Active visibility
    , visibilityLink Completed visibility
    ]

visibilityLink ∷ Visibility → Visibility → H.Html Action
visibilityLink v1 v2 =
  H.li []
    [ H.a
        [ H.classes ("selected" <$ guard (v1 == v2))
        , H.href (visibilityHref v1)
        ]
        [ H.text (show v1) ]
    ]

visibilityHref ∷ Visibility → String
visibilityHref = case _ of
  All       → "#/"
  Active    → "#/active"
  Completed → "#/completed"

renderClear ∷ Int → H.Html Action
renderClear len =
  H.button
    [ H.classes [ "clear-completed" ]
    , H.hidden (len == 0)
    , H.onClick (H.always_ DeleteCompleted)
    ]
    [ H.text $ "Clear completed (" <> show len <> ")" ]

infoFooter ∷ forall i. H.Html i
infoFooter =
  H.footer
    [ H.classes [ "info" ] ]
    [ H.p [] [ H.text "Double-click to edit a todo" ]
    , H.p []
        [ H.text "Written by "
        , H.a [ H.href "https://github.com/natefaubion" ] [ H.text "Nathan Faubion" ]
        ]
    , H.p []
        [ H.text "Part of "
        , H.a [ H.href "http://todomvc.com" ] [ H.text "TodoMVC" ]
        ]
    ]

onEnter ∷ forall i r. i → H.IProp (onKeyDown ∷ DOM.KeyboardEvent | r) i
onEnter a = H.onKeyDown \ev →
  if DOM.key ev == "Enter"
    then Just a
    else Nothing

styleHidden ∷ forall r i. Boolean → H.IProp (style ∷ String | r) i
styleHidden =
  if _
    then H.styles [ H.Style "visibility" "hidden" ]
    else H.styles [ H.Style "visibility" "visibility" ]

app ∷ Maybe StoredModel → App.App Effect (Const Void) Model Action
app storedModel =
  { render
  , update
  , subs: const mempty
  , init: App.purely model
  }
  where
  model = case storedModel of
    Nothing → initialModel
    Just sm → initialModel
      { todos = sm.todos
      , fresh = sm.fresh
      }

type StoredModel =
  { todos ∷ Array Todo
  , fresh ∷ Int
  }

storageKey ∷ String
storageKey = "todos"

data Effect a
  = Focus DOM.Element a
  | WriteStorage Model a

derive instance functorEffect ∷ Functor Effect

runEffect ∷ forall eff. Effect ~> Eff (dom ∷ DOM | eff)
runEffect = case _ of
  Focus el next → do
    F.for_ (DOM.fromElement el) DOM.focus
    pure next
  WriteStorage model next → do
    let
      storedModel =
        { todos: model.todos
        , fresh: model.fresh
        }
    DOM.window
      >>= DOM.localStorage
      >>= DOM.setItem storageKey (writeJSON storedModel)
    pure next

routeAction ∷ String → Maybe Action
routeAction = case _ of
  "/"          → Just $ ChangeVisibility All
  "/active"    → Just $ ChangeVisibility Active
  "/completed" → Just $ ChangeVisibility Completed
  _            → Nothing

main ∷ Eff (App.AppEffects ()) Unit
main = do
  storedModel ←
    DOM.window
      >>= DOM.localStorage
      >>= DOM.getItem storageKey
      >>> map (_ >>= readJSON >>> hush)

  driver ←
    App.runWithSelector
      (toBasicEff runEffect)
      (app storedModel)
      "#app"

  hashes \oldHash newHash →
    F.for_ (routeAction newHash) driver
