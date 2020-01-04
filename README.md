# Shine - Declarative Graphics for the Web

[![Build Status](https://travis-ci.org/fgaz/shine.svg?branch=master)](https://travis-ci.org/fgaz/shine)

Shine wraps javascript's drawing functions in a declarative API.

Heavily inspired by [gloss](http://gloss.ouroborus.net/).

**demo** (compiled shine-examples) [here](http://fgaz.github.io/shine/shine-examples)

## Compiling

You need [ghcjs](https://github.com/ghcjs/ghcjs)

A great way of building ghcjs packages is to use the
[reflex platform](https://github.com/reflex-frp/reflex-platform)

## Usage

### `Picture`s

To represent your drawing you have to build a tree using the `Picture` datatype.

```haskell
pic :: Picture
pic = Rect 10 20 -- represents a 10x20 square
```

To compose multiple `Picture`s you can use `Over`, which accepts two `Picture`s
and overlaps them.

`Picture` is a monoid: `<>` is an alias for `Over` and `mempty` is the empty picture.

```haskell
-- draw some shapes on top of each other
pic :: Picture
pic = Rect 10 20
   <> Translate 30 30 (Circle 15)
   <> Colored (Color 255 0 0 0.2) (RectF 4 4)
   <> Text "Sans 12px" LeftAlign 200 "The quick brown fox jumps over the lazy dog."
```

Using `Foldable` you can do things like

```haskell
concentricCircles :: Picture
concentricCircles = foldMap Circle [1,10..100]
```

### Drawing `Picture`s

Before drawing anything you need to obtain a `CanvasRenderingContext2D` (and sometimes a `Document`).
For this purpose, shine provides two utility functions: `fullScreenCanvas` and `fixedSizeCanvas`

```haskell
{-# LANGUAGE CPP #-}

import Graphics.Shine
import Graphics.Shine.Input
import Graphics.Shine.Picture

import GHCJS.DOM (currentDocumentUnchecked)

-- This is how the ghcjs-dom hello-world does it.
-- It's boilerplate, so in the next shine version there
-- will probably be a ready-to-use run function
#if defined(ghcjs_HOST_OS)
run :: a -> a
run = id
#elif defined(MIN_VERSION_jsaddle_wkwebview)
import Language.Javascript.JSaddle.WKWebView (run)
#else
import Language.Javascript.JSaddle.WebKitGTK (run)
#endif

main :: IO ()
main = run $ do
    doc <- currentDocumentUnchecked -- use currentDocument to handle failure
    ctx <- fixedSizeCanvas doc 400 400
    -- do something with ctx (and maybe doc)
```

To render a `Picture` on a context you have three options:

#### `render`

You can draw it manually using `render` from `Graphics.Shine.Render`

```haskell
main :: IO ()
    {- omitted: get the context, see before -}
    render ctx concentricCircles
```

#### `animate`

You can draw a `Picture` that depends on time. That is, a `Float -> Picture`.

```haskell
-- An expanding-and-contracting circle.
animation :: Float -> Picture
animation = Translate 200 200
          . Circle
          . (*100) . (+1) -- bigger positive oscillation
          . sin -- the circle's radius oscillates

main :: IO ()
main =  do
    {- omitted: get the context, see before -}
    animate ctx 30 animation
```

#### `play`

Finally, you can draw a `Picture` that depends on time, inputs
(keyboard and mouse) and an internal state. This is especially useful for games,
hence the name.

```haskell
-- this code draws a black rectangle in the center of the canvas only when the
-- left mouse button is pressed
main :: IO ()
main = do
    {- omitted: get the context and the document, see before -}
    play ctx doc 30 initialState draw handleInput step
  where
    -- our state represents the state of the left mouse button
    initialState = Up
    -- we draw a square only if the button is pressed
    draw Up = Empty
    draw Down = Translate 200 200 $ RectF 200 200
    -- when an event is fired we store the button state
    handleInput (MouseBtn BtnLeft buttonState _) = const buttonState
    handleInput _ = id -- catch-all for all other events
    step _ = id -- our state does not depend on time
```

### Examples

See the [`shine-examples`](./shine-examples) package
([Hackage](https://hackage.haskell.org/package/shine-examples)).

### Handling assets

Handling assets with ghcjs and the browser is somewhat tricky.

The usual way of doing it for normal haskell programs is to use
[`data-files`](https://www.haskell.org/cabal/users-guide/developing-packages.html#pkg-field-data-files).
Unfortunately, this work by either hardcoding an absolute path in the executable,
which will not work when the program is put on a server in a different path,
or with an environment variable override (`$pkgname_datadir`),
which will not work because
[environment variables do not exist in the browser](https://github.com/ghcjs/shims/blob/de9560ee1fb8d1ca58c12da20c71a778eb08f3db/src/environment.js#L155).

So, for now, the solution is to explicitly specify absolute or relative paths
in the source code, and then to manyully copy the assets to the appropriate
location after building/deploying the code.
This is what I did in the `animated-shapes` demo for the peppers image.

Relevant cabal ticket, discussing a possible new way of handling assets:
https://github.com/haskell/cabal/issues/6096#issuecomment-570784857

