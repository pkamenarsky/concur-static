-- | SVG elements
--
-- See: <https://developer.mozilla.org/en-US/docs/Web/SVG/Element>
module Concur.Static.SVG where

-- Note that this module is auto-generated.
-- See @./misc/gen-svg-modules@ for details.

import Concur.Static (Props, VDOM)
import Concur.Static.DOM (elWithNamespace)

-- | Helper function for creating SVG elements.
el :: Bounded a => Enum a => String -> [Props a] -> [VDOM a] -> VDOM a
el = elWithNamespace (Just "http://www.w3.org/2000/svg")

-- * SVG Elements

animate :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
animate = el "animate"

animateMotion :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
animateMotion = el "animateMotion"

animateTransform :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
animateTransform = el "animateTransform"

circle :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
circle = el "circle"

clipPath :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
clipPath = el "clipPath"

colorProfile :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
colorProfile = el "color-profile"

defs :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
defs = el "defs"

desc :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
desc = el "desc"

discard :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
discard = el "discard"

ellipse :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
ellipse = el "ellipse"

feBlend :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
feBlend = el "feBlend"

feColorMatrix :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
feColorMatrix = el "feColorMatrix"

feComponentTransfer :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
feComponentTransfer = el "feComponentTransfer"

feComposite :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
feComposite = el "feComposite"

feConvolveMatrix :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
feConvolveMatrix = el "feConvolveMatrix"

feDiffuseLighting :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
feDiffuseLighting = el "feDiffuseLighting"

feDisplacementMap :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
feDisplacementMap = el "feDisplacementMap"

feDistantLight :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
feDistantLight = el "feDistantLight"

feDropShadow :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
feDropShadow = el "feDropShadow"

feFlood :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
feFlood = el "feFlood"

feFuncA :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
feFuncA = el "feFuncA"

feFuncB :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
feFuncB = el "feFuncB"

feFuncG :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
feFuncG = el "feFuncG"

feFuncR :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
feFuncR = el "feFuncR"

feGaussianBlur :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
feGaussianBlur = el "feGaussianBlur"

feImage :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
feImage = el "feImage"

feMerge :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
feMerge = el "feMerge"

feMergeNode :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
feMergeNode = el "feMergeNode"

feMorphology :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
feMorphology = el "feMorphology"

feOffset :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
feOffset = el "feOffset"

fePointLight :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
fePointLight = el "fePointLight"

feSpecularLighting :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
feSpecularLighting = el "feSpecularLighting"

feSpotLight :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
feSpotLight = el "feSpotLight"

feTile :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
feTile = el "feTile"

feTurbulence :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
feTurbulence = el "feTurbulence"

filter :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
filter = el "filter"

foreignObject :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
foreignObject = el "foreignObject"

g :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
g = el "g"

hatch :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
hatch = el "hatch"

hatchpath :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
hatchpath = el "hatchpath"

image :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
image = el "image"

line :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
line = el "line"

linearGradient :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
linearGradient = el "linearGradient"

marker :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
marker = el "marker"

mask :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
mask = el "mask"

mesh :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
mesh = el "mesh"

meshgradient :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
meshgradient = el "meshgradient"

meshpatch :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
meshpatch = el "meshpatch"

meshrow :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
meshrow = el "meshrow"

metadata :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
metadata = el "metadata"

mpath :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
mpath = el "mpath"

path :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
path = el "path"

pattern :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
pattern = el "pattern"

polygon :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
polygon = el "polygon"

polyline :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
polyline = el "polyline"

radialGradient :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
radialGradient = el "radialGradient"

rect :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
rect = el "rect"

script :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
script = el "script"

set :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
set = el "set"

solidcolor :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
solidcolor = el "solidcolor"

stop :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
stop = el "stop"

style :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
style = el "style"

svg :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
svg = el "svg"

switch :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
switch = el "switch"

symbol :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
symbol = el "symbol"

text :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
text = el "text"

textPath :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
textPath = el "textPath"

title :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
title = el "title"

tspan :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
tspan = el "tspan"

unknown :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
unknown = el "unknown"

use :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
use = el "use"

view :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a
view = el "view"
