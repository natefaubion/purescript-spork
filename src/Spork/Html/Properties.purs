module Spork.Html.Properties
  ( module Spork.Html.Properties
  , module I
  ) where

import Prelude
import Data.MediaType (MediaType(..)) as I
import Data.String as String
import DOM.HTML.Indexed (CSSPixel) as I
import DOM.HTML.Indexed.ButtonType (ButtonType(..)) as I
import DOM.HTML.Indexed.CrossOriginValue (CrossOriginValue(..)) as I
import DOM.HTML.Indexed.DirValue (DirValue(..)) as I
import DOM.HTML.Indexed.FormMethod (FormMethod(..)) as I
import DOM.HTML.Indexed.InputType (InputType(..)) as I
import DOM.HTML.Indexed.KindValue (KindValue(..)) as I
import DOM.HTML.Indexed.MenuitemType (MenuitemType(..)) as I
import DOM.HTML.Indexed.MenuType (MenuType(..)) as I
import DOM.HTML.Indexed.OnOff (OnOff(..)) as I
import DOM.HTML.Indexed.OrderedListType (OrderedListType(..)) as I
import DOM.HTML.Indexed.PreloadValue (PreloadValue(..)) as I
import DOM.HTML.Indexed.ScopeValue (ScopeValue(..)) as I
import DOM.HTML.Indexed.StepValue (StepValue(..)) as I
import DOM.HTML.Indexed.WrapValue (WrapValue(..)) as I
import Spork.Html.Core (IProp, class ToPropValue)
import Spork.Html.Core as Html

data Style = Style String String

action ∷ ∀ r i. String → IProp (action ∷ String | r) i
action = Html.prop "action"

alt ∷ ∀ r i. String → IProp (alt ∷ String | r) i
alt = Html.prop "alt"

autocomplete ∷ ∀ r i. Boolean → IProp (autocomplete ∷ I.OnOff | r) i
autocomplete = Html.prop "autocomplete" <<< if _ then I.On else I.Off

autofocus ∷ ∀ r i. Boolean → IProp (autofocus ∷ Boolean | r) i
autofocus = Html.prop "autofocus"

charset ∷ ∀ r i. String → IProp (charset ∷ String | r) i
charset = Html.prop "charset"

checked ∷ ∀ r i. Boolean → IProp (checked ∷ Boolean | r) i
checked = Html.prop "checked"

classes ∷ ∀ r i. Array String → IProp (class ∷ String | r) i
classes = Html.prop "className" <<< String.joinWith " "

cols ∷ ∀ r i. Int → IProp (cols ∷ Int | r) i
cols = Html.prop "cols"

colSpan ∷ ∀ r i. Int → IProp (colSpan ∷ Int | r) i
colSpan = Html.prop "colSpan"

disabled ∷ ∀ r i. Boolean → IProp (disabled ∷ Boolean | r) i
disabled = Html.prop "disabled"

draggable ∷ ∀ r i. Boolean → IProp (draggable ∷ Boolean | r) i
draggable = Html.prop "draggable"

enabled ∷ ∀ r i. Boolean → IProp (disabled ∷ Boolean | r) i
enabled = Html.prop "enabled" <<< not

enctype ∷ ∀ r i. String → IProp (enctype ∷ String | r) i
enctype = Html.prop "enctype"

for ∷ ∀ r i. String → IProp (for ∷ String | r) i
for = Html.prop "htmlFor"

height ∷ ∀ r i. I.CSSPixel → IProp (height ∷ I.CSSPixel | r) i
height = Html.prop "height"

hidden ∷ ∀ r i. Boolean → IProp (hidden ∷ Boolean | r) i
hidden = Html.prop "hidden"

href ∷ ∀ r i. String → IProp (href ∷ String | r) i
href = Html.prop "href"

id_ ∷ ∀ r i. String → IProp (id ∷ String | r) i
id_ = Html.prop "id"

method ∷ ∀ r i. I.FormMethod → IProp (method ∷ I.FormMethod | r) i
method = Html.prop "method"

multiple ∷ ∀ r i. Boolean → IProp (multiple ∷ Boolean | r) i
multiple = Html.prop "multiple"

name ∷ ∀ r i. String → IProp (name ∷ String | r) i
name = Html.prop "name"

noValidate ∷ ∀ r i. Boolean → IProp (noValidate ∷ Boolean | r) i
noValidate = Html.prop "noValidate"

placeholder ∷ ∀ r i. String → IProp (placeholder ∷ String | r) i
placeholder = Html.prop "placeholder"

readOnly ∷ ∀ r i. Boolean → IProp (readOnly ∷ Boolean | r) i
readOnly = Html.prop "readOnly"

rel ∷ ∀ r i. String → IProp (rel ∷ String | r) i
rel = Html.prop "rel"

required ∷ ∀ r i. Boolean → IProp (required ∷ Boolean | r) i
required = Html.prop "required"

rows ∷ ∀ r i. Int → IProp (rows ∷ Int | r) i
rows = Html.prop "rows"

rowSpan ∷ ∀ r i. Int → IProp (rowSpan ∷ Int | r) i
rowSpan = Html.prop "rowSpan"

spellcheck ∷ ∀ r i. Boolean → IProp (spellcheck ∷ Boolean | r) i
spellcheck = Html.prop "spellcheck"

src ∷ ∀ r i. String → IProp (src ∷ String | r) i
src = Html.prop "src"

style ∷ ∀ r i. String → IProp (style ∷ String | r) i
style = Html.prop "style"

styles ∷ ∀ r i. Array Style → IProp (style ∷ String | r) i
styles = style <<< String.joinWith ";" <<< map \(Style prop val) → prop <> ":" <> val

tabIndex ∷ ∀ r i. Int → IProp (tabIndex ∷ Int | r) i
tabIndex = Html.prop "tabIndex"

target ∷ ∀ r i. String → IProp (target ∷ String | r) i
target = Html.prop "target"

title ∷ ∀ r i. String → IProp (title ∷ String | r) i
title = Html.prop "title"

type_ ∷ ∀ value r i. ToPropValue value ⇒ value → IProp (type ∷ value | r) i
type_ = Html.prop "type"

value ∷ ∀ r i. String → IProp (value ∷ String | r) i
value = Html.prop "value"

width ∷ ∀ r i. I.CSSPixel → IProp (width ∷ I.CSSPixel | r) i
width = Html.prop "width"
