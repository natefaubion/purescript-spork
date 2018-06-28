module Subs.Main where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Effect (Effect)
import Spork.App as App
import Spork.Html as H
import Spork.Interpreter (merge, never)
import Subs.Sub (Sub)
import Subs.Sub as Sub

type Model =
  { subscribing ∷ Boolean
  , coord ∷ Maybe Sub.Coord
  }

initialModel ∷ Model
initialModel =
  { subscribing: false
  , coord: Nothing
  }

data Action
  = UpdateSub Boolean
  | UpdateCoord Sub.Coord

update ∷ Model → Action → App.Transition (Const Void) Model Action
update model = case _ of
  UpdateSub subscribing →
    App.purely
      { subscribing
      , coord: Nothing
      }

  UpdateCoord coord →
    App.purely $ model { coord = Just coord }

render ∷ Model → H.Html Action
render { subscribing, coord } =
  H.div
    [ H.classes [ "wrapper" ] ]
    [ H.button
        [ H.type_ H.ButtonButton
        , H.onClick (H.always_ (UpdateSub (not subscribing)))
        ]
        [ H.text
            if subscribing
              then "Unsubscribe"
              else "Track the Pointer"
        ]
    , H.span
        [ H.classes [ guard subscribing "subscribing" ] ]
        [ H.text
            if subscribing
               then (maybe "Move the pointer" renderCoord coord)
               else ""
        ]
    ]

renderCoord ∷ Sub.Coord → String
renderCoord { x, y } = "(" <> show x <> ", " <> show y <> ")"

subs ∷ Model → App.Batch Sub Action
subs { subscribing }
  | subscribing = App.lift (Sub.mouseMove UpdateCoord)
  | otherwise   = mempty

app ∷ App.App (Const Void) Sub Model Action
app =
  { render
  , update
  , subs
  , init: App.purely initialModel
  }

main ∷ Effect Unit
main = do
  inst ←
    App.makeWithSelector
      (never `merge` Sub.interpreter)
      app
      "#app"

  inst.run
