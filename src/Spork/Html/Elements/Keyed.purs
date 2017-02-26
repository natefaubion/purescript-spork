module Spork.Html.Elements.Keyed where

import Data.Tuple (Tuple)
import DOM.HTML.Indexed as I
import Spork.Html.Core (Html)
import Spork.Html.Core as Html

type Node r i = Array (Html.IProp r i) → Array (Tuple String (Html i)) → Html i

div ∷ ∀ i. Node I.HTMLdiv i
div = Html.keyed "div"

dl ∷ ∀ i. Node I.HTMLdl i
dl = Html.keyed "dl"

form ∷ ∀ i. Node I.HTMLform i
form = Html.keyed "form"

ol ∷ ∀ i. Node I.HTMLol i
ol = Html.keyed "ol"

section ∷ ∀ i. Node I.HTMLsection i
section = Html.keyed "section"

table ∷ ∀ i. Node I.HTMLtable i
table = Html.keyed "table"

tbody ∷ ∀ i. Node I.HTMLtbody i
tbody = Html.keyed "tbody"

tfoot ∷ ∀ i. Node I.HTMLtfoot i
tfoot = Html.keyed "tfoot"

th ∷ ∀ i. Node I.HTMLth i
th = Html.keyed "th"

thead ∷ ∀ i. Node I.HTMLthead i
thead = Html.keyed "thead"

tr ∷ ∀ i. Node I.HTMLtr i
tr = Html.keyed "tr"

ul ∷ ∀ i. Node I.HTMLul i
ul = Html.keyed "ul"
