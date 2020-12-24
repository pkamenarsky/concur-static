# concur-static

## General

`concur-static` explores, similar to [`concur-replica`](https://github.com/pkamenarsky/concur-replica), yet another point in the [Concur](https://github.com/ajnsit/concur) UI space. It is a static UI generator, yet it provides more dynamism than other similar projects (i.e. a finite counter can be programmed with ease); and less than full-blown SPA frameworks, like React (an *infinite* counter is currently impossible).

It does so by expecting all runtime values to be *bounded* and *enumerable*, so that the complete UI state space can be inspected in advance, at generation time. The output is a single `html` file with all UI transitions encoded statically, in Javascript.

Possible use cases include static sites, blogs, documentation (imagine interactive SVG diagrams or code examples), dynamic reports - and who knows what else?

## Quick start

```haskell
import Concur.Static
import Concur.Static.DOM

import Prelude hiding (div)

main = writeFile "out.html" $ generateModule $
  div [] [ text "Hello!" ]
```

Open `out.html` and enjoy the greeting.

## A quick excursion into the Concur model

*Note:* For a longer and better exposition, see the official [Concur documentation](https://github.com/ajnsit/concur-documentation/blob/master/README.md). If you're already familiar with Concur you might still want to read this section, since `concur-static` (necessarily) diverges a bit in its implementation.

Let's start with a simple Concur component:

```haskell
needyButton = do
  button [ onClick () ] [ text "Click me" ]
  text "Thanks"
```

The exact types of `button` and `text` are not that important at the moment; just know that every DOM element (with the exception of `text`, which renders a text node) takes a list of `Props` (which events to listen to, as well as general HTML attributes, like `style` or `disabled`) and a list of children.

The main insight to be taken away is that every line represents a *sequential step* in the execution of a component. Every such step replaces the previous step and is a *complete description* of the component's state at that point in time. Execution continues when an event that is listened for is fired, in this case `onClick`.

This is important, so here is the code and the actual UI once again, with time flowing downwards, side by side:

```
                                                      ┏━━━━━━━━━━┓
1.     button [ onClick () ] [ text "Click me" ]      ┃ Click me ┃ ━━━┓ onClick
                                                      ┗━━━━━━━━━━┛    ┃
                                                                      ┃
2.     text "Thanks"                                  "Thanks"     ◄━━┛

```

Yet another way to describe this program more abstractly is by way of a sequence diagram:

```
   ━━━
1.  ●
2.  ●
   ━━━
```

where every `●` represents an irreducible sequential step (we'll see why this is helpful in a minute).

So far this isn't terribly interesting. However, and this is the raison d'être of the general [Concur model](https://github.com/ajnsit/concur), 1. components can run *in parallel*; and 2. when a component finishes *all of its siblings are killed*, so that execution proceeds with the next step after the parallel composition.

An example might bring some clarity:

```haskell
twoNeedyButtons = div [] -- A
  [ button [ onClick () ] [ text "Click me" ] -- B
  , button [ onClick () ] [ text "Click me" ] -- C
  ]
```

The children (in this case `B` and `C`) of a DOM component (`A`) always run in parallel and so this program shows a `div` containing two buttons side by side. See its sequence diagram:

```
━━━A━━━
━B━ ━C━
 ●   ●
━┳━ ━┳━
━━━┳━━━
```

Noticeably, if `B` finishes first, `C` is discarded and execution continues with the next step in `A` - and since there is no next step in this case, `A` ends as well. Same goes for the other case, `C`. Note that both `B` and `C` *can evolve independently*; it's only when one ends that the other is mercilessly killed.

Some components never finish, like `text` (since it can't listen for events). This is represented by an uninterrupted dash (`━`) after the last step in the sequence diagrams, whereas `┳` denotes an ending component.

One more example to drive the point home:

```haskell
moreNeedyButtons = div [] -- A
  [ button [ onClick () ] [ text "Click me" ]    -- B
  , do
       button [ onClick () ] [ text "Click me" ] -- C
       text "Thanks"
  ]
```

```
━━━A━━━
━B━ ━C━
 ●   ●
     ●
━┳━ ━━━
━━━┳━━━
```

Here the only way for `A` to end is if `B` ends. Whenever `C` reaches the `text` step it will stay there forever.

### Return values

Components run in the `VDOM` monad and by extension always return a value:

```haskell
-- Components
text   :: String -> VDOM a
div    :: [Props a] -> [VDOM a] -> VDOM a
button :: [Props a] -> [VDOM a] -> VDOM a

-- Props
onClick :: a -> Props a
```

Each component accepts a list of `Props` and children, all parameterised over the same type `a`. Said otherwise, all children (and their own children, and so on) must return the same type! How is it then possible to discern where an event originated from?

Notice the first argument to `onClick` - that's the return value of the DOM component after a click and can be used as a differentiator:

```haskell
data Choice = One | Two deriving (Enum, Bounded)

choice = do
  r <- div []
    [ button [ onClick One ] [ text "Button A" ]
    , button [ onClick Two ] [ text "Button B" ]
    ]
  case r of
    One -> text "You clicked A"
    Two -> text "You clicked B"
```

When the return value is not important it's easiest to pass `()` to all event handlers, as in the moody button examples above.

### Looping components

By now, it shouldn't be too hard to program the Hello World of every new UI framework: a counter. In general Concur, one might do:

```haskell
counter x = do
  div [ onClick () ] [ text (show x) ]
  counter (x + 1)
```

```
┏━━━┓
┃   ▼
┃  ━━━
┃   ●
┃  ━┳━
┗━━━┛
```

But attempting to generate an output for `counter` with `concur-static` would lead to an infinite loop and an ever-growing RAM consumption. This is because `concur-static` exhausts the complete UI state space of a program by descending into every possible branch and generating static, imperative commands for every transition between states. Obviously, general unbounded recursion is a no-go.

However, a *bounded* counter is super easy:

```haskell
counter x
  | x > 10 = pure ()
  | otherwise = do
      div [ onClick () ] [ text (show x) ]
      counter (x + 1)
```

Quite a respectable feat for a static UI generator, one might claim!

And there's more: even *unbounded* recursion is quite possible, if the component has zero arguments. "Tying the knot" is achieved explicitly by way of the `loop` combinator:

```haskell
goodAndEvil = loop $ \recur -> do
  button [ onClick () ] [ text "Good" ]
  button [ onClick () ] [ text "Evil" ]
  recur
```

```
┏━━━┓
┃   ▼
┃  ━━━
┃   ●
┃   ●
┃  ━┳━
┗━━━┛
```

This will generate a program that switches between good and evil indefinitely - but never goes beyond.

Mutual recursion is also possible with `loopMany`:

```haskell
mutual = loopMany [a, b]
  where
    a [recurA, recurB] = do
      r <- div []
        [ button [ onClick (Left ()) ] [ text "This is A, stay here" ]
        , button [ onClick (Right ()) ] [ text "Go to B" ]
        ]
      case r of
        Left _ -> recurA
        Right _ -> recurB

    b [recurA, recurB] = do
      r <- div []
        [ button [ onClick (Left ()) ] [ text "This is B, stay here" ]
        , button [ onClick (Right ()) ] [ text "Go to A" ]
        ]
      case r of
        Left _ -> recurB
        Right _ -> recurA
```

## Restrictions

How does `concur-replica` work, exactly? In fact, the type signatures from above weren't the full story. Here are they, now complete:

```haskell
button :: Bounded a => Enum a => [Props a] -> [VDOM a] -> VDOM a

-- Props
onClick :: Bounded a => Enum a => a -> Props a
```

So, every return type must be *finite* - i.e. bounded and enumerable, making it possible to evaluate the complete UI state space. The generated embedded Javascript code then looks similar to this:

```javascript
function _62(k, parent, index) {
  const e = document.createElement('div');
  parent.insertBefore(e, parent.childNodes[index]);

  function next(r) {
    parent.removeChild(e);
    switch (r) {
      case 0: _59(k, parent, index); break;
      case 1: _61(k, parent, index); break;
    }
  };

  // Events
  e.addEventListener('click', function () { next(0) });

  // Children
  _56(next, e, 0);
  _57(next, e, 1);
}
```

The magic happens in the `switch` clause - since all return values of a component are known in advance it is always possible to dispatch dynamically to the correct predetermined continuation. This is heaven for Laplace's daemon!

As a side note, every component is completely teared down before the next one is built up. Therein lies an opportunity for optimisation: since all DOM configurations are known beforehand, it would be better to compute a diff and subsequently generate only minimal patching code between UI states.

Note that some Haskell types, although in principle bounded and enumerable, are "too big" - for example, `Int`. It's not advisable to return such types from a `concur-static` component as you'll quickly run out of memory; in the future a new type class is in order.

`concur-static` also provides `Bounded` and `Enum` instances for `Either`, which is useful for wrapping return values from other components:

```haskell
insistentButton title = button [ onClick () ] [ text title ]

insistentButtons = loop $ \recur -> do
  r <- div []
    [ Left  <$> insistentButton "Button A"
    , Right <$> insistentButton "Button B"
    ]
  case r of
    Left _ -> do
      div [ onClick ] [ text "You clicked A, but you must click B!" ]
      recur
    Right _ -> text "You clicked B, good."
```

```
┏━━━┓
┃   ▼
┃  ━━━━━━━
┃  ━━━ ━━━
┃   ●   ●
┃   ●   ●
┃  ━┳━ ━━━
┃  ━┳━━━━━
┗━━━┛
```

## Roadmap

* Implement a better type class for "small finite types"
* Routing and other such things
* Diff VDOM
* Minimise JS

## Footnote

For the curious, the Concur model is a form of [synchronous programming](https://en.wikipedia.org/wiki/Synchronous_programming_language), at least in spirit. I am convinced that this idea is fundamental and applicable to a wide range of contexts, as evidenced by the fact that a paradigm mainly used in embedded programming and circuit design lends itself so well to UIs.

Network programming deserves a mention here as well - the synchronous paradigm turns implementing complex communication protocols almost into an enjoyable pastime!

For a more in-depth exploration and an attempted port of the [Céu](http://www.ceu-lang.org) language to Haskell, check out my **very WIP** [Synchron](https://github.com/pkamenarsky/synchron) project.
