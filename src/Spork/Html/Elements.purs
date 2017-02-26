module Spork.Html.Elements where

import DOM.HTML.Indexed as I
import Spork.Html.Core (Html)
import Spork.Html.Core as Html

type Node r i = Array (Html.IProp r i) → Array (Html i) → Html i

type Leaf r i = Array (Html.IProp r i) → Html i

a ∷ ∀ i. Node I.HTMLa i
a = Html.elem "a"

abbr ∷ ∀ i. Node I.HTMLabbr i
abbr = Html.elem "abbr"

address ∷ ∀ i. Node I.HTMLaddress i
address = Html.elem "address"

area ∷ ∀ i. Leaf I.HTMLarea i
area ps = Html.elem "area" ps []

article ∷ ∀ i. Node I.HTMLarticle i
article = Html.elem "article"

aside ∷ ∀ i. Node I.HTMLaside i
aside = Html.elem "aside"

audio ∷ ∀ i. Node I.HTMLaudio i
audio = Html.elem "audio"

b ∷ ∀ i. Node I.HTMLb i
b = Html.elem "b"

base ∷ ∀ i. Leaf I.HTMLbase i
base ps = Html.elem "base" ps []

bdi ∷ ∀ i. Node I.HTMLbdi i
bdi = Html.elem "bdi"

bdo ∷ ∀ i. Node I.HTMLbdo i
bdo = Html.elem "bdo"

blockquote ∷ ∀ i. Node I.HTMLblockquote i
blockquote = Html.elem "blockquote"

body ∷ ∀ i. Node I.HTMLbody i
body = Html.elem "body"

br ∷ ∀ i. Leaf I.HTMLbr i
br ps = Html.elem "br" ps []

button ∷ ∀ i. Node I.HTMLbutton i
button = Html.elem "button"

canvas ∷ ∀ i. Leaf I.HTMLcanvas i
canvas ps = Html.elem "canvas" ps []

caption ∷ ∀ i. Node I.HTMLcaption i
caption = Html.elem "caption"

cite ∷ ∀ i. Node I.HTMLcite i
cite = Html.elem "cite"

code ∷ ∀ i. Node I.HTMLcode i
code = Html.elem "code"

col ∷ ∀ i. Leaf I.HTMLcol i
col ps = Html.elem "col" ps []

colgroup ∷ ∀ i. Node I.HTMLcolgroup i
colgroup = Html.elem "colgroup"

command ∷ ∀ i. Leaf I.HTMLcommand i
command ps = Html.elem "command" ps []

datalist ∷ ∀ i. Node I.HTMLdatalist i
datalist = Html.elem "datalist"

dd ∷ ∀ i. Node I.HTMLdd i
dd = Html.elem "dd"

del ∷ ∀ i. Node I.HTMLdel i
del = Html.elem "del"

details ∷ ∀ i. Node I.HTMLdetails i
details = Html.elem "details"

dfn ∷ ∀ i. Node I.HTMLdfn i
dfn = Html.elem "dfn"

dialog ∷ ∀ i. Node I.HTMLdialog i
dialog = Html.elem "dialog"

div ∷ ∀ i. Node I.HTMLdiv i
div = Html.elem "div"

dl ∷ ∀ i. Node I.HTMLdl i
dl = Html.elem "dl"

dt ∷ ∀ i. Node I.HTMLdt i
dt = Html.elem "dt"

em ∷ ∀ i. Node I.HTMLem i
em = Html.elem "em"

embed ∷ ∀ i. Node I.HTMLembed i
embed = Html.elem "embed"

fieldset ∷ ∀ i. Node I.HTMLfieldset i
fieldset = Html.elem "fieldset"

figcaption ∷ ∀ i. Node I.HTMLfigcaption i
figcaption = Html.elem "figcaption"

figure ∷ ∀ i. Node I.HTMLfigure i
figure = Html.elem "figure"

footer ∷ ∀ i. Node I.HTMLfooter i
footer = Html.elem "footer"

form ∷ ∀ i. Node I.HTMLform i
form = Html.elem "form"

h1 ∷ ∀ i. Node I.HTMLh1 i
h1 = Html.elem "h1"

h2 ∷ ∀ i. Node I.HTMLh2 i
h2 = Html.elem "h2"

h3 ∷ ∀ i. Node I.HTMLh3 i
h3 = Html.elem "h3"

h4 ∷ ∀ i. Node I.HTMLh4 i
h4 = Html.elem "h4"

h5 ∷ ∀ i. Node I.HTMLh5 i
h5 = Html.elem "h5"

h6 ∷ ∀ i. Node I.HTMLh6 i
h6 = Html.elem "h6"

head ∷ ∀ i. Node I.HTMLhead i
head = Html.elem "head"

header ∷ ∀ i. Node I.HTMLheader i
header = Html.elem "header"

hr ∷ ∀ i. Leaf I.HTMLhr i
hr ps = Html.elem "hr" ps []

html ∷ ∀ i. Node I.HTMLhtml i
html = Html.elem "html"

i ∷ ∀ i. Node I.HTMLi i
i = Html.elem "i"

iframe ∷ ∀ i. Leaf I.HTMLiframe i
iframe ps = Html.elem "iframe" ps []

img ∷ ∀ i. Leaf I.HTMLimg i
img ps = Html.elem "img" ps []

input ∷ ∀ i. Leaf I.HTMLinput i
input ps = Html.elem "input" ps []

ins ∷ ∀ i. Node I.HTMLins i
ins = Html.elem "ins"

kbd ∷ ∀ i. Node I.HTMLkbd i
kbd = Html.elem "kbd"

label ∷ ∀ i. Node I.HTMLlabel i
label = Html.elem "label"

li ∷ ∀ i. Node I.HTMLli i
li = Html.elem "li"

link ∷ ∀ i. Leaf I.HTMLlink i
link ps = Html.elem "link" ps []

main ∷ ∀ i. Node I.HTMLmain i
main = Html.elem "main"

map ∷ ∀ i. Node I.HTMLmap i
map = Html.elem "map"

mark ∷ ∀ i. Node I.HTMLmark i
mark = Html.elem "mark"

menu ∷ ∀ i. Node I.HTMLmenu i
menu = Html.elem "menu"

menuitem ∷ ∀ i. Node I.HTMLmenuitem i
menuitem = Html.elem "menuitem"

meta ∷ ∀ i. Leaf I.HTMLmeta i
meta ps = Html.elem "meta" ps []

meter ∷ ∀ i. Node I.HTMLmeter i
meter = Html.elem "meter"

nav ∷ ∀ i. Node I.HTMLnav i
nav = Html.elem "nav"

noscript ∷ ∀ i. Node I.HTMLnoscript i
noscript = Html.elem "noscript"

object ∷ ∀ i. Node I.HTMLobject i
object = Html.elem "object"

ol ∷ ∀ i. Node I.HTMLol i
ol = Html.elem "ol"

optgroup ∷ ∀ i. Node I.HTMLoptgroup i
optgroup = Html.elem "optgroup"

option ∷ ∀ i. Node I.HTMLoption i
option = Html.elem "option"

output ∷ ∀ i. Node I.HTMLoutput i
output = Html.elem "output"

p ∷ ∀ i. Node I.HTMLp i
p = Html.elem "p"

param ∷ ∀ i. Leaf I.HTMLparam i
param ps = Html.elem "param" ps []

pre ∷ ∀ i. Node I.HTMLpre i
pre = Html.elem "pre"

progress ∷ ∀ i. Node I.HTMLprogress i
progress = Html.elem "progress"

q ∷ ∀ i. Node I.HTMLq i
q = Html.elem "q"

rp ∷ ∀ i. Node I.HTMLrp i
rp = Html.elem "rp"

rt ∷ ∀ i. Node I.HTMLrt i
rt = Html.elem "rt"

ruby ∷ ∀ i. Node I.HTMLruby i
ruby = Html.elem "ruby"

samp ∷ ∀ i. Node I.HTMLsamp i
samp = Html.elem "samp"

script ∷ ∀ i. Node I.HTMLscript i
script = Html.elem "script"

section ∷ ∀ i. Node I.HTMLsection i
section = Html.elem "section"

select ∷ ∀ i. Node I.HTMLselect i
select = Html.elem "select"

small ∷ ∀ i. Node I.HTMLsmall i
small = Html.elem "small"

source ∷ ∀ i. Leaf I.HTMLsource i
source ps = Html.elem "source" ps []

span ∷ ∀ i. Node I.HTMLspan i
span = Html.elem "span"

strong ∷ ∀ i. Node I.HTMLstrong i
strong = Html.elem "strong"

style ∷ ∀ i. Node I.HTMLstyle i
style = Html.elem "style"

sub ∷ ∀ i. Node I.HTMLsub i
sub = Html.elem "sub"

summary ∷ ∀ i. Node I.HTMLsummary i
summary = Html.elem "summary"

sup ∷ ∀ i. Node I.HTMLsup i
sup = Html.elem "sup"

table ∷ ∀ i. Node I.HTMLtable i
table = Html.elem "table"

tbody ∷ ∀ i. Node I.HTMLtbody i
tbody = Html.elem "tbody"

td ∷ ∀ i. Node I.HTMLtd i
td = Html.elem "td"

textarea ∷ ∀ i. Leaf I.HTMLtextarea i
textarea ps = Html.elem "textarea" ps []

tfoot ∷ ∀ i. Node I.HTMLtfoot i
tfoot = Html.elem "tfoot"

th ∷ ∀ i. Node I.HTMLth i
th = Html.elem "th"

thead ∷ ∀ i. Node I.HTMLthead i
thead = Html.elem "thead"

time ∷ ∀ i. Node I.HTMLtime i
time = Html.elem "time"

title ∷ ∀ i. Node I.HTMLtitle i
title = Html.elem "title"

tr ∷ ∀ i. Node I.HTMLtr i
tr = Html.elem "tr"

track ∷ ∀ i. Leaf I.HTMLtrack i
track ps = Html.elem "track" ps []

u ∷ ∀ i. Node I.HTMLu i
u = Html.elem "u"

ul ∷ ∀ i. Node I.HTMLul i
ul = Html.elem "ul"

var ∷ ∀ i. Node I.HTMLvar i
var = Html.elem "var"

video ∷ ∀ i. Node I.HTMLvideo i
video = Html.elem "video"

wbr ∷ ∀ i. Leaf I.HTMLwbr i
wbr ps = Html.elem "wbr" ps []
