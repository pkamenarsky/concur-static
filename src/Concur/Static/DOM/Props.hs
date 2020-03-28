module Concur.Static.DOM.Props where

import Concur.Static (Props (Attr, Style), AValue (AString, ABool))
import Data.List (intercalate)

boolProp :: String -> Bool -> Props a
boolProp k v = Attr k (ABool v)

textProp :: String -> String -> Props a
textProp k v = Attr k (AString v)

key :: String -> Props a
key v = textProp "key" v

style :: [(String, String)] -> Props a
style = Style

-- | Define multiple classes conditionally
--
-- > div [ classList [ ("empty", null items) ] [ ]
--
classList ::  [(String, Bool)] -> Props a
classList xs = textProp "class" $ intercalate " " [ t | (t, True) <- xs ]

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/title>
title ::  String -> Props a
title = textProp "title"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/selected>
selected ::  Bool -> Props a
selected = boolProp "selected"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/hidden>
hidden ::  Bool -> Props a
hidden             = boolProp "hidden"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/value>
value ::  String -> Props a
value             = textProp "value"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/defaultValue>
defaultValue ::  String -> Props a
defaultValue      = textProp "defaultValue"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/accept>
accept ::  String -> Props a
accept            = textProp "accept"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/acceptCharset>
acceptCharset ::  String -> Props a
acceptCharset     = textProp "acceptCharset"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/a>
a_ ::  String -> Props a
a_            = textProp "a"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/autocomplete>
autocomplete ::  Bool -> Props a
autocomplete b = textProp "autocomplete" (if b then "on" else "off")

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/autosave>
autosave ::  String -> Props a
autosave          = textProp "autosave"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/disabled>
disabled ::  Bool -> Props a
disabled          = boolProp "disabled"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/enctype>
enctype ::  String -> Props a
enctype           = textProp "enctype"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/formation>
formation ::  String -> Props a
formation         = textProp "formation"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/list>
list ::  String -> Props a
list              = textProp "list"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/maxlength>
maxlength ::  String -> Props a
maxlength         = textProp "maxlength"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/minlength>
minlength ::  String -> Props a
minlength         = textProp "minlength"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/method>
method ::  String -> Props a
method            = textProp "method"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/multiple>
multiple ::  Bool -> Props a
multiple          = boolProp "multiple"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/novalidate>
novalidate ::  Bool -> Props a
novalidate        = boolProp "noValidate"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/pattern>
pattern ::  String -> Props a
pattern           = textProp "pattern"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/readonly>
readonly ::  Bool -> Props a
readonly          = boolProp "readOnly"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/required>
required ::  Bool -> Props a
required          = boolProp "required"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/size>
size ::  String -> Props a
size              = textProp "size"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/for>
for ::  String -> Props a
for               = textProp "for"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/form>
form_ ::  String -> Props a
form_               = textProp "form"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/max>
max ::  String -> Props a
max               = textProp "max"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/min>
min ::  String -> Props a
min               = textProp "min"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/step>
step ::  String -> Props a
step              = textProp "step"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/cols>
cols ::  String -> Props a
cols              = textProp "cols"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/rows>
rows ::  String -> Props a
rows              = textProp "rows"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/wrap>
wrap ::  String -> Props a
wrap              = textProp "wrap"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/target>
target_ ::  String -> Props a
target_            = textProp "target"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/download>
download ::  String -> Props a
download          = textProp "download"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/downloadAs>
downloadAs ::  String -> Props a
downloadAs        = textProp "downloadAs"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/hreflang>
hreflang ::  String -> Props a
hreflang          = textProp "hreflang"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/media>
media ::  String -> Props a
media             = textProp "media"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/ping>
ping ::  String -> Props a
ping              = textProp "ping"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/rel>
rel ::  String -> Props a
rel               = textProp "rel"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/ismap>
ismap ::  String -> Props a
ismap             = textProp "ismap"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/usemap>
usemap ::  String -> Props a
usemap            = textProp "usemap"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/shape>
shape ::  String -> Props a
shape             = textProp "shape"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/coords>
coords ::  String -> Props a
coords            = textProp "coords"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/src>
src ::  String -> Props a
src               = textProp "src"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/height>
height ::  String -> Props a
height            = textProp "height"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/width>
width ::  String -> Props a
width             = textProp "width"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/alt>
alt ::  String -> Props a
alt               = textProp "alt"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/autoplay>
autoplay ::  Bool -> Props a
autoplay          = boolProp "autoplay"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/controls>
controls ::  Bool -> Props a
controls          = boolProp "controls"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/loop>
loop_ ::  Bool -> Props a
loop_              = boolProp "loop"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/preload>
preload ::  String -> Props a
preload           = textProp "preload"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/poster>
poster ::  String -> Props a
poster            = textProp "poster"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/default>
default_ ::  Bool -> Props a
default_           = boolProp "default"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/kind>
kind ::  String -> Props a
kind              = textProp "kind"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/srclang>
srclang ::  String -> Props a
srclang           = textProp "srclang"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/sandbox>
sandbox ::  String -> Props a
sandbox           = textProp "sandbox"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/seamless>
seamless ::  String -> Props a
seamless          = textProp "seamless"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/srcdoc>
srcdoc ::  String -> Props a
srcdoc            = textProp "srcdoc"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/reversed>
reversed ::  String -> Props a
reversed          = textProp "reversed"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/start>
start ::  String -> Props a
start             = textProp "start"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/align>
align ::  String -> Props a
align             = textProp "align"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/colspan>
colspan ::  String -> Props a
colspan           = textProp "colspan"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/rowspan>
rowspan ::  String -> Props a
rowspan           = textProp "rowspan"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/headers>
headers ::  String -> Props a
headers           = textProp "headers"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/scope>
scope ::  String -> Props a
scope             = textProp "scope"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/async>
async ::  String -> Props a
async             = textProp "async"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/charset>
charset ::  String -> Props a
charset           = textProp "charset"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/content>
content ::  String -> Props a
content           = textProp "content"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/defer>
defer ::  String -> Props a
defer             = textProp "defer"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/httpEquiv>
httpEquiv ::  String -> Props a
httpEquiv         = textProp "httpEquiv"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/language>
language ::  String -> Props a
language          = textProp "language"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/scoped>
scoped ::  String -> Props a
scoped            = textProp "scoped"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/type>
type_ ::  String -> Props a
type_ = textProp "type"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/name>
name ::  String -> Props a
name = textProp "name"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/href>
href ::  String -> Props a
href = textProp "href"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/id>
id ::  String -> Props a
id = textProp "id"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/placeholder>
placeholder ::  String -> Props a
placeholder = textProp "placeholder"

-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/checked>
checked ::  Bool -> Props a
checked = boolProp "checked"

-- | Set "autofocus" property
-- <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Props/autofocus>
autofocus ::  Bool -> Props a
autofocus = boolProp "autofocus"

-- | Set "className" property
-- <https://developer.mozilla.org/en-US/docs/Web/API/Element/className>
className ::  String -> Props a
className = textProp "class"
