module Crud.Route
  ( Route(..)
  , CrudRoute(..)
  , routes
  , parseRoute
  , printRoute
  , printRouteHash
  , link
  ) where

import Prelude

import DOM.HTML.Indexed (HTMLa)
import Data.Either (hush)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe)
import Routing (match)
import Routing.Match (Match)
import Routing.Match.Class (end, int, lit)
import Spork.Html as H

data Route
  = Home
  | Contacts (CrudRoute Int)

data CrudRoute a
  = CrudIndex
  | CrudCreate
  | CrudUpdate a
  | CrudRead a

routes ∷ Match Route
routes =
  oneOf
    [ Home     <$  end
    , Contacts <$> crudRoute "contacts" int
    ]

crudRoute ∷ forall a. String → Match a → Match (CrudRoute a)
crudRoute path resourceId =
  lit path *> oneOf
    [ CrudIndex  <$  end
    , CrudCreate <$  (lit "create" <* end)
    , CrudUpdate <$> (resourceId <* lit "edit" <* end)
    , CrudRead   <$> (resourceId <* end)
    ]

parseRoute ∷ String → Maybe Route
parseRoute = hush <<< match routes

printRoute ∷ Route → String
printRoute = case _ of
  Home       → ""
  Contacts c → printCrudRoute "contacts" show c

printRouteHash ∷ Route → String
printRouteHash = append "#" <<< printRoute

printCrudRoute ∷ forall a. String → (a → String) → CrudRoute a → String
printCrudRoute path printResource = case _ of
  CrudIndex    → path <> "/"
  CrudCreate   → path <> "/create"
  CrudUpdate r → path <> "/" <> printResource r <> "/edit"
  CrudRead r   → path <> "/" <> printResource r

link ∷ forall a. Route → Array (H.IProp HTMLa a) → Array (H.Html a) → H.Html a
link route props = H.a (props <> [ H.href (printRouteHash route) ])
