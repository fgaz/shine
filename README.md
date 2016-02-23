# Shine <small>(let me know if you think of a better name)</small> - Declarative Graphics for the Web

Heavily inspired by `gloss`

**Work In Progress!**

## Compiling

You'll need [`ghcjs`](https://github.com/ghcjs/ghcjs)

## Usage

`shine` wraps javascript's drawing functions in a declarative api.

### `Picture`s

To draw something you have to build a tree representing your drawing using
the `Picture` datatype.

```
pic = Rect 10 20 -- represents a 10x20 square
```

To compose multiple `Picture`s you can use `Over`, which accepts two `Picture`s
and overlaps them.

`Picture` is a monoid: `<>` is an alias for `Over` and `mempty` is the empty picture.

```
-- draw some shapes on top of each other
pic = Rect 10 20
   <> Translate 30 30 (Circle 15)
   <> Colored (Color 255 0 0 0.2) (RectF 4 4)
   <> Text "Sans 12px" LeftAlign 200 "The quick brown fox jumps over the lazy dog."
```

Using `Foldable`s you can do things like

```
concentricCircles = foldMap Circle [1,10..100]
```

### Drawing `Picture`s

To render a `Picture` on a canvas you have three options:

#### `render`

You can draw it manually using `render` from `Graphics.Shine.Render`

#### `animate`

TODO

#### `play`

TODO
