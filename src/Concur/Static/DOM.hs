module Concur.Static.DOM where

import Concur.Static (Props, VDOM, view, DOM (Element, Text))

el :: Bounded a => Enum a => String -> [Props a] -> [VDOM a] -> VDOM a
el = elWithNamespace Nothing

elWithNamespace :: Bounded a => Enum a => Maybe String -> String -> [Props a] -> [VDOM a] -> VDOM a
elWithNamespace ns name props children = view $ Element ns name props children

unit :: VDOM () -> VDOM ()
unit = id

text :: Bounded a => Enum a => String -> VDOM a
text = view . Text

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div
div :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
div  = el "div"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table
table :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
table  = el "table"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/thead
thead :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
thead  = el "thead"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tbody
tbody :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
tbody  = el "tbody"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr
tr :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
tr  = el "tr"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/th
th :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
th  = el "th"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td
td :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
td  = el "td"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tfoot
tfoot :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
tfoot  = el "tfoot"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/section
section :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
section  = el "section"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/header
header :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
header  = el "header"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/footer
footer :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
footer  = el "footer"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button
button :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
button = el "button"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form
form :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
form = el "form"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/p
p :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
p = el "p"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/s
s :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
s = el "s"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul
ul :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
ul = el "ul"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/span
span :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
span = el "span"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/strong
strong :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
strong = el "strong"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li
li :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
li = el "li"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h1
h1 :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
h1 = el "h1"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h2
h2 :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
h2 = el "h2"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h3
h3 :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
h3 = el "h3"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h4
h4 :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
h4 = el "h4"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h5
h5 :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
h5 = el "h5"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h6
h6 :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
h6 = el "h6"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hr
hr :: Bounded a => Enum a => [Props a] -> VDOM a
hr = flip (el "hr") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/pre
pre :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
pre = el "pre"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input
input :: Bounded a => Enum a => [Props a] -> VDOM a
input = flip (el "input") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/label
label :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
label = el "label"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a
a :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
a = el "a"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/mark
mark :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
mark = el "mark"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ruby
ruby :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
ruby = el "ruby"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rt
rt :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
rt = el "rt"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rp
rp :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
rp = el "rp"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdi
bdi :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
bdi = el "bdi"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdo
bdo :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
bdo = el "bdo"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/wbr
wbr :: Bounded a => Enum a => [Props a] -> VDOM a
wbr = flip (el "wbr") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details
details :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
details = el "details"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/summary
summary :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
summary = el "summary"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menuitem
menuitem :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
menuitem = el "menuitem"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menu
menu :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
menu = el "menu"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/fieldset
fieldset :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
fieldset = el "fieldset"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/legend
legend :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
legend = el "legend"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/datalist
datalist :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
datalist = el "datalist"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/optgroup
optgroup :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
optgroup = el "optgroup"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/keygen
keygen :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
keygen = el "keygen"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/output
output :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
output = el "output"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/progress
progress :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
progress = el "progress"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meter
meter :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
meter = el "meter"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/center
center :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
center = el "center"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio
audio :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
audio = el "audio"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video
video :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
video = el "video"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/source
source :: Bounded a => Enum a => [Props a] -> VDOM a
source = flip (el "source") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/track
track :: Bounded a => Enum a => [Props a] -> VDOM a
track = flip (el "track") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/embed
embed :: Bounded a => Enum a => [Props a] -> VDOM a
embed = flip (el "embed") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/object
object :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
object = el "object"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/param
param :: Bounded a => Enum a => [Props a] -> VDOM a
param = flip (el "param") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ins
ins :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
ins = el "ins"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/del
del :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
del = el "del"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/small
small :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
small = el "small"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/cite
cite :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
cite = el "cite"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dfn
dfn :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
dfn = el "dfn"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/abbr
abbr :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
abbr = el "abbr"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time
time :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
time = el "time"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/var
var :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
var = el "var"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/samp
samp :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
samp = el "samp"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/kbd
kbd :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
kbd = el "kbd"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/caption
caption :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
caption = el "caption"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/colgroup
colgroup :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
colgroup = el "colgroup"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/col
col :: Bounded a => Enum a => [Props a] -> VDOM a
col = flip (el "col") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/nav
nav :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
nav = el "nav"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/article
article :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
article = el "article"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/aside
aside :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
aside = el "aside"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/address
address :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
address = el "address"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/main
main_ :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
main_ = el "main"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/body
body :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
body = el "body"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figure
figure :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
figure = el "figure"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figcaption
figcaption :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
figcaption = el "figcaption"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl
dl :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
dl = el "dl"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dt
dt :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
dt = el "dt"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd
dd :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
dd = el "dd"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img
img :: Bounded a => Enum a => [Props a] -> VDOM a
img = flip (el "img") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe
iframe :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
iframe = el "iframe"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/canvas
canvas :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
canvas = el "canvas"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/math
math :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
math = el "math"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select
select :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
select = el "select"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option
option :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
option = el "option"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea
textarea :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
textarea = el "textarea"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sub
sub :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
sub = el "sub"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sup
sup :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
sup = el "sup"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/br
br :: Bounded a => Enum a => [Props a] -> VDOM a
br = flip (el "br") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ol
ol :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
ol = el "ol"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/blockquote
blockquote :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
blockquote = el "blockquote"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/code
code :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
code = el "code"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/em
em :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
em = el "em"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/i
i :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
i = el "i"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/b
b :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
b = el "b"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/u
u :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
u = el "u"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/q
q :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
q = el "q"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script
script :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
script = el "script"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link
link :: Bounded a => Enum a => [Props a] -> VDOM a
link = flip (el "link") []
