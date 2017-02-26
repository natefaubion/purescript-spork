module Spork.Html.Events where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign (Foreign, toForeign, F)
import Data.Foreign (readBoolean, readInt, readString) as F
import Data.Foreign.Index (prop) as F
import Data.Maybe (Maybe(..))
import DOM.Event.Types (Event, MouseEvent, KeyboardEvent, FocusEvent)
import DOM.HTML.Event.Types (DragEvent)
import Spork.Html.Core (on, IProp)
import Unsafe.Coerce (unsafeCoerce)

always ∷ ∀ i a. (a → i) → a → Maybe i
always = compose Just

always_ ∷ ∀ i a. i → a → Maybe i
always_ = const <<< Just

onAbort ∷ ∀ r i. (Event → Maybe i) → IProp (onAbort ∷ Event | r) i
onAbort = on "abort"

onBlur ∷ ∀ r i. (FocusEvent → Maybe i) → IProp (onBlur ∷ FocusEvent | r) i
onBlur = on "blur" <<< unsafeCoerce

onChange ∷ ∀ r i. (Event → Maybe i) → IProp (onChange ∷ Event | r) i
onChange = on "change"

onClick ∷ ∀ r i. (MouseEvent → Maybe i) → IProp (onClick ∷ MouseEvent | r) i
onClick = on "click" <<< unsafeCoerce

onContextMenu ∷ ∀ r i. (MouseEvent → Maybe i) → IProp (onContextMenu ∷ MouseEvent | r) i
onContextMenu = on "contextmenu" <<< unsafeCoerce

onDoubleClick ∷ ∀ r i. (MouseEvent → Maybe i) → IProp (onDoubleClick ∷ MouseEvent | r) i
onDoubleClick = on "dblclick" <<< unsafeCoerce

onDrag ∷ ∀ r i. (DragEvent → Maybe i) → IProp (onDrag ∷ DragEvent | r) i
onDrag = on "drag" <<< unsafeCoerce

onDragEnd ∷ ∀ r i. (DragEvent → Maybe i) → IProp (onDragEnd ∷ DragEvent | r) i
onDragEnd = on "dragend" <<< unsafeCoerce

onDragEnter ∷ ∀ r i. (DragEvent → Maybe i) → IProp (onDragEnter ∷ DragEvent | r) i
onDragEnter = on "dragenter" <<< unsafeCoerce

onDragExit ∷ ∀ r i. (DragEvent → Maybe i) → IProp (onDragExit ∷ DragEvent | r) i
onDragExit = on "dragexit" <<< unsafeCoerce

onDragLeave ∷ ∀ r i. (DragEvent → Maybe i) → IProp (onDragLeave ∷ DragEvent | r) i
onDragLeave = on "dragleave" <<< unsafeCoerce

onDragOver ∷ ∀ r i. (DragEvent → Maybe i) → IProp (onDragOver ∷ DragEvent | r) i
onDragOver = on "dragover" <<< unsafeCoerce

onDragStart ∷ ∀ r i. (DragEvent → Maybe i) → IProp (onDragStart ∷ DragEvent | r) i
onDragStart = on "dragstart" <<< unsafeCoerce

onDrop ∷ ∀ r i. (DragEvent → Maybe i) → IProp (onDrop ∷ DragEvent | r) i
onDrop = on "drop" <<< unsafeCoerce

onError ∷ ∀ r i. (Event → Maybe i) → IProp (onError ∷ Event | r) i
onError = on "error"

onFocus ∷ ∀ r i. (FocusEvent → Maybe i) → IProp (onFocus ∷ FocusEvent | r) i
onFocus = on "focus" <<< unsafeCoerce

onFocusIn ∷ ∀ r i. (FocusEvent → Maybe i) → IProp (onFocusIn ∷ FocusEvent | r) i
onFocusIn = on "focusin" <<< unsafeCoerce

onFocusOut ∷ ∀ r i. (FocusEvent → Maybe i) → IProp (onFocusOut ∷ FocusEvent | r) i
onFocusOut = on "focusout" <<< unsafeCoerce

onInput ∷ ∀ r i. (Event → Maybe i) → IProp (onInput ∷ Event | r) i
onInput = on "input"

onInvalid ∷ ∀ r i. (Event → Maybe i) → IProp (onInvalid ∷ Event | r) i
onInvalid = on "invalid"

onKeyDown ∷ ∀ r i. (KeyboardEvent → Maybe i) → IProp (onKeyDown ∷ KeyboardEvent | r) i
onKeyDown = on "keydown" <<< unsafeCoerce

onKeyPress ∷ ∀ r i. (KeyboardEvent → Maybe i) → IProp (onKeyPress ∷ KeyboardEvent | r) i
onKeyPress = on "keypress" <<< unsafeCoerce

onKeyUp ∷ ∀ r i. (KeyboardEvent → Maybe i) → IProp (onKeyUp ∷ KeyboardEvent | r) i
onKeyUp = on "keyup" <<< unsafeCoerce

onLoad ∷ ∀ r i. (Event → Maybe i) → IProp (onLoad ∷ Event | r) i
onLoad = on "load"

onMouseDown ∷ ∀ r i. (MouseEvent → Maybe i) → IProp (onMouseDown ∷ MouseEvent | r) i
onMouseDown = on "mousedown" <<< unsafeCoerce

onMouseEnter ∷ ∀ r i. (MouseEvent → Maybe i) → IProp (onMouseEnter ∷ MouseEvent | r) i
onMouseEnter = on "mouseenter" <<< unsafeCoerce

onMouseLeave ∷ ∀ r i. (MouseEvent → Maybe i) → IProp (onMouseLeave ∷ MouseEvent | r) i
onMouseLeave = on "mouseleave" <<< unsafeCoerce

onMouseMove ∷ ∀ r i. (MouseEvent → Maybe i) → IProp (onMouseMove ∷ MouseEvent | r) i
onMouseMove = on "mousemove" <<< unsafeCoerce

onMouseOver ∷ ∀ r i. (MouseEvent → Maybe i) → IProp (onMouseOver ∷ MouseEvent | r) i
onMouseOver = on "mouseover" <<< unsafeCoerce

onMouseOut ∷ ∀ r i. (MouseEvent → Maybe i) → IProp (onMouseOut ∷ MouseEvent | r) i
onMouseOut = on "mouseout" <<< unsafeCoerce

onMouseUp ∷ ∀ r i. (MouseEvent → Maybe i) → IProp (onMouseUp ∷ MouseEvent | r) i
onMouseUp = on "mouseup" <<< unsafeCoerce

onReset ∷ ∀ r i. (Event → Maybe i) → IProp (onReset ∷ Event | r) i
onReset = on "reset"

onScroll ∷ ∀ r i. (Event → Maybe i) → IProp (onScroll ∷ Event | r) i
onScroll = on "scroll"

onSelect ∷ ∀ r i. (Event → Maybe i) → IProp (onSelect ∷ Event | r) i
onSelect = on "select"

onSubmit ∷ ∀ r i. (Event → Maybe i) → IProp (onSubmit ∷ Event | r) i
onSubmit = on "submit"

onTransitionEnd ∷ ∀ r i. (Event → Maybe i) → IProp (onTransitionEnd ∷ Event | r) i
onTransitionEnd = on "transitionend"

type ForeignDecoder a = Foreign → F a

currentTargetValue ∷ ForeignDecoder String
currentTargetValue =
  F.prop "currentTarget"
  >=> F.prop "value"
  >=> F.readString

foreignHandler ∷ ∀ a r i. ForeignDecoder a → String → (a → Maybe i) → IProp r i
foreignHandler decoder ty handler = on ty handler'
  where
    handler' ev =
      case runExcept (decoder (toForeign ev)) of
        Left _  → Nothing
        Right a → handler a

onValueChange ∷ ∀ r i. (String → Maybe i) → IProp (value ∷ String, onChange ∷ Event | r) i
onValueChange = foreignHandler currentTargetValue "change"

onValueInput ∷ ∀ r i. (String → Maybe i) → IProp (value ∷ String, onInput ∷ Event | r) i
onValueInput = foreignHandler currentTargetValue "input"

onSelectedIndexChange ∷ ∀ r i. (Int → Maybe i) → IProp (selectedIndex ∷ Int, onChange ∷ Event | r) i
onSelectedIndexChange = foreignHandler decoder "change"
  where
    decoder =
      F.prop "currentTarget"
      >=> F.prop "selectedIndex"
      >=> F.readInt

onChecked ∷ ∀ r i. (Boolean → Maybe i) → IProp (checked ∷ Boolean, onChange ∷ Event | r) i
onChecked = foreignHandler decoder "change"
  where
    decoder =
      F.prop "currentTarget"
      >=> F.prop "checked"
      >=> F.readBoolean
