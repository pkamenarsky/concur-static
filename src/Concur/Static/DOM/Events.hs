module Concur.Static.DOM.Events where

import Concur.Static (Props (Event))

--------------------------------------------------------------------------------

-- | `blur` event defined with custom options
--
-- <https://developer.mozilla.org/en-US/docs/Web/Events/blur>
--
onBlur :: Bounded a => Enum a => a -> Props a
onBlur = Event "blur"

-- | https://developer.mozilla.org/en-US/docs/Web/Events/click
onClick :: Bounded a => Enum a => a -> Props a
onClick = Event "click"

-- | https://developer.mozilla.org/en-US/docs/Web/Events/focus
onFocus :: Bounded a => Enum a => a -> Props a
onFocus = Event "focus"

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dblclick
onDoubleClick :: Bounded a => Enum a => a -> Props a
onDoubleClick = Event "dblClick"

-- | https://developer.mozilla.org/en-US/docs/Web/Events/input
onInput :: Bounded a => Enum a => a -> Props a
onInput = Event "input"

-- | https://developer.mozilla.org/en-US/docs/Web/Events/change
onChange :: Bounded a => Enum a => a -> Props a
onChange = Event "change"

-- | https://developer.mozilla.org/en-US/docs/Web/Events/keydown
onKeyDown :: Bounded a => Enum a => a -> Props a
onKeyDown = Event "keyDown"

-- | https://developer.mozilla.org/en-US/docs/Web/Events/keypress
onKeyPress :: Bounded a => Enum a => a -> Props a
onKeyPress = Event "keyPress"

-- | https://developer.mozilla.org/en-US/docs/Web/Events/keyup
onKeyUp :: Bounded a => Enum a => a -> Props a
onKeyUp = Event "keyUp"

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseup
onMouseUp :: Bounded a => Enum a => a -> Props a
onMouseUp = Event "mouseUp"

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mousedown
onMouseDown :: Bounded a => Enum a => a -> Props a
onMouseDown = Event "mouseDown"

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseenter
onMouseEnter :: Bounded a => Enum a => a -> Props a
onMouseEnter = Event "mouseEnter"

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseleave
onMouseLeave :: Bounded a => Enum a => a -> Props a
onMouseLeave = Event "mouseLeave"

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseover
onMouseOver :: Bounded a => Enum a => a -> Props a
onMouseOver = Event "mouseOver"

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseout
onMouseOut :: Bounded a => Enum a => a -> Props a
onMouseOut = Event "mouseOut"

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragstart
onDragStart :: Bounded a => Enum a => a -> Props a
onDragStart = Event "dragStart"

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragover
onDragOver :: Bounded a => Enum a => a -> Props a
onDragOver = Event "dragOver"

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragend
onDragEnd :: Bounded a => Enum a => a -> Props a
onDragEnd = Event "dragEnd"

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragenter
onDragEnter :: Bounded a => Enum a => a -> Props a
onDragEnter = Event "dragEnter"

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragleave
onDragLeave :: Bounded a => Enum a => a -> Props a
onDragLeave = Event "dragLeave"

-- | https://developer.mozilla.org/en-US/docs/Web/Events/drag
onDrag :: Bounded a => Enum a => a -> Props a
onDrag = Event "drag"
