module Spork.Html.Core
  ( Html
  , ElementRef
  , IProp (..)
  , text
  , elem
  , keyed
  , class ToPropValue
  , toPropValue
  , prop
  , attr
  , on
  , ref
  , lazy
  , lazy2
  , lazy3
  , memoized
  , empty
  , when
  , module Exports
  ) where

import Prelude
import Data.Bifunctor (bimap)
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple)
import DOM.Event.Types (EventType(..), Event) as DOM
import DOM.HTML.Indexed.ButtonType (ButtonType, renderButtonType)
import DOM.HTML.Indexed.CrossOriginValue (CrossOriginValue, renderCrossOriginValue)
import DOM.HTML.Indexed.DirValue (DirValue, renderDirValue)
import DOM.HTML.Indexed.FormMethod (FormMethod, renderFormMethod)
import DOM.HTML.Indexed.InputType (InputType, renderInputType)
import DOM.HTML.Indexed.KindValue (KindValue, renderKindValue)
import DOM.HTML.Indexed.MenuitemType (MenuitemType, renderMenuitemType)
import DOM.HTML.Indexed.MenuType (MenuType, renderMenuType)
import DOM.HTML.Indexed.OnOff (OnOff, renderOnOff)
import DOM.HTML.Indexed.OrderedListType (OrderedListType, renderOrderedListType)
import DOM.HTML.Indexed.PreloadValue (PreloadValue, renderPreloadValue)
import DOM.HTML.Indexed.ScopeValue (ScopeValue, renderScopeValue)
import DOM.HTML.Indexed.StepValue (StepValue, renderStepValue)
import DOM.HTML.Indexed.WrapValue (WrapValue, renderWrapValue)
import DOM.Node.Types (Element) as DOM
import Halogen.VDom as V
import Halogen.VDom.DOM.Prop as P
import Halogen.VDom.DOM.Prop (ElemRef(..)) as Exports
import Spork.Html.Thunk (Thunk, thunked, thunk1, thunk2, thunk3)
import Unsafe.Coerce (unsafeCoerce)

type HtmlV i = V.VDom (Array (P.Prop i)) (Thunk Html i)

type ElementRef = P.ElemRef DOM.Element

newtype Html i = Html (HtmlV i)

derive instance newtypeHtml ∷ Newtype (Html i) _

instance functorHtml ∷ Functor Html where
  map f (Html vdom) = Html (bimap (map (map f)) (map f) vdom)

unwrapF ∷ ∀ f i. f (Html i) → f (HtmlV i)
unwrapF = unsafeCoerce

unwrapG ∷ ∀ f g i. f (g (Html i)) → f (g (HtmlV i))
unwrapG = unsafeCoerce

class ToPropValue a where
  toPropValue ∷ a → P.PropValue

instance stringToPropValue ∷ ToPropValue String where
  toPropValue = P.propFromString

instance intToPropValue ∷ ToPropValue Int where
  toPropValue = P.propFromInt

instance numberToPropValue ∷ ToPropValue Number where
  toPropValue = P.propFromNumber

instance booleanToPropValue ∷ ToPropValue Boolean where
  toPropValue = P.propFromBoolean

instance mediaTypeToPropValue :: ToPropValue MediaType where
  toPropValue = P.propFromString <<< unwrap

instance buttonTypeToPropValue :: ToPropValue ButtonType where
  toPropValue = P.propFromString <<< renderButtonType

instance crossOriginValueToPropValue :: ToPropValue CrossOriginValue where
  toPropValue = P.propFromString <<< renderCrossOriginValue

instance dirValueToPropValue :: ToPropValue DirValue where
  toPropValue = P.propFromString <<< renderDirValue

instance formMethodToPropValue :: ToPropValue FormMethod where
  toPropValue = P.propFromString <<< renderFormMethod

instance inputTypeToPropValue :: ToPropValue InputType where
  toPropValue = P.propFromString <<< renderInputType

instance kindValueToPropValue :: ToPropValue KindValue where
  toPropValue = P.propFromString <<< renderKindValue

instance menuitemTypeToPropValue :: ToPropValue MenuitemType where
  toPropValue = P.propFromString <<< renderMenuitemType

instance menuTypeToPropValue :: ToPropValue MenuType where
  toPropValue = P.propFromString <<< renderMenuType

instance onOffToPropValue :: ToPropValue OnOff where
  toPropValue = P.propFromString <<< renderOnOff

instance orderedListTypeToPropValue :: ToPropValue OrderedListType where
  toPropValue = P.propFromString <<< renderOrderedListType

instance preloadValueToPropValue :: ToPropValue PreloadValue where
  toPropValue = P.propFromString <<< renderPreloadValue

instance scopeValueToPropValue :: ToPropValue ScopeValue where
  toPropValue = P.propFromString <<< renderScopeValue

instance stepValueToPropValue :: ToPropValue StepValue where
  toPropValue = P.propFromString <<< renderStepValue

instance wrapValueToPropValue :: ToPropValue WrapValue where
  toPropValue = P.propFromString <<< renderWrapValue

newtype IProp (r ∷ # Type) i = IProp (P.Prop i)

text ∷ ∀ i. String → Html i
text = Html <<< V.Text

elem ∷ ∀ r i. String → Array (IProp r i) → Array (Html i) → Html i
elem name props children =
  Html
    (V.Elem
      (V.ElemSpec Nothing (V.ElemName name) (unsafeCoerce props ∷ Array (P.Prop i)))
      (unwrapF children))

keyed ∷ ∀ r i. String → Array (IProp r i) → Array (Tuple String (Html i)) → Html i
keyed name props children =
  Html
    (V.Keyed
      (V.ElemSpec Nothing (V.ElemName name) (unsafeCoerce props ∷ Array (P.Prop i)))
      (unwrapG children))

prop ∷ ∀ r i a. ToPropValue a ⇒ String → a → IProp r i
prop n = IProp <<< P.Property n <<< toPropValue

attr ∷ ∀ r i. String → String → IProp r i
attr n = IProp <<< P.Attribute Nothing n

on ∷ ∀ r i. String → (DOM.Event → Maybe i) → IProp r i
on ty = IProp <<< P.Handler (DOM.EventType ty)

ref ∷ ∀ r i. (P.ElemRef DOM.Element → Maybe i) → IProp r i
ref = IProp <<< P.Ref

-- | Lazily renders a subtree given the same render function and argument.
lazy ∷ ∀ a i. (a → Html i) → a → Html i
lazy f a = Html (V.Widget (Fn.runFn2 thunk1 f a))

-- | Like `lazy`, but allows two arguments.
lazy2 ∷ ∀ a b i. (a → b → Html i) → a → b → Html i
lazy2 f a b = Html (V.Widget (Fn.runFn3 thunk2 f a b))

-- | Like `lazy`, but allows three arguments.
lazy3 ∷ ∀ a b c i. (a → b → c → Html i) → a → b → c → Html i
lazy3 f a b c = Html (V.Widget (Fn.runFn4 thunk3 f a b c))

-- | Creates a render function which will lazily render it's subtree
-- | according to the given equality predicate. Arguments determined to be
-- | equal will not be re-rendered. The usage of this function is slightly
-- | Different than `lazy` in that you must apply it at the top-level of
-- | module.
memoized ∷ ∀ a i. (a → a → Boolean) → (a → Html i) → a → Html i
memoized eqFn f = Html <<< V.Widget <$> thunked eqFn f

empty ∷ ∀ i. Html i
empty = text ""

when ∷ ∀ i. Boolean → (Unit → Html i) → Html i
when bool render =
  if bool
    then render unit
    else empty
