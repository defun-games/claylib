# claylib
A Common Lisp 2D/3D game toolkit built on top of [Raylib](https://www.raylib.com/) 4.0.

## Quick Start
Claylib is not yet in Quicklisp. Load claylib.asd and run `(ql:quickload :claylib/examples)` or similar to load all of the available packages. See the next section for what those packages are and how you might use them.

To see the [examples](/examples) in action, just run e.g. `(example-core-01)`. Hit Escape to exit. At the time of this writing, only the [core examples](https://www.raylib.com/examples.html) are done, and a few of those are still missing.

## Packages
This repo contains four separate packages/systems:
- `claylib/wrap` wraps Raylib and Raymath via [cl-autowrap](https://github.com/rpav/cl-autowrap), along with a few small fixes. There's probably no reason to use it directly.
- `claylib/ll` is a thin layer on top of `claylib/wrap` that adds some convenience features but mostly keeps the C semantics. If you have a lot of experience with Raylib in C or you just really like manual memory management, then `claylib/ll` is for you.
- `claylib` sits atop `claylib/ll` and does a lot of work to try to smooth out and abstract the C semantics away from the user. It's not perfect, nor _can_ it be (more on that later). But its goal is to feel as Lispy as possible, and no Lispier!
- `claylib/examples` contains a number of -- you guessed it -- examples, remixed from [Raylib's own](https://www.raylib.com/examples.html).

## Current Status
The project should be considered **beta**. Development is active and API's are subject to change.

`claylib/ll` should be highly usable, as a thin wrapper over Raylib. Only Raylib and Raymath are included, so you will be missing things like 2D physics, but you can always use other libraries for that.

For `claylib`, you're best off reviewing the examples as a survey of what's done and what's not. 2D support is largely complete; 3D support is a bit more iffy. If any piece is particularly important to you, please file an [issue](https://github.com/defun-games/claylib/issues) and we will prioritize it!

## Using `claylib`
Walking through the first example will give a "boilerplate" of how you might make a game with `claylib`. (This is certainly not the only way, just _a_ way.)

### `with-window`
```
(defun example-core-01 ()
  (with-window (:title "raylib [core] example - basic window")
    ...))
```
The `with-window` macro initializes a new Raylib window and takes care of freeing stray C memory when it closes. Here we set the window title with the `:title` keyword. Other keywords:
- `:width` -- window width in pixels, an integer
- `:height` -- window height in pixels, an integer
- `:fps` -- target frames-per-second, an integer
- `:flags` -- a list of window flag constants (of the form `+flag-whatever+`)
- `:min-size` -- the minimum window size, a two-element list `(width height)`

### Scenes
```
(let ((scene (make-scene ()
                         `((text ...))))))
```
"Scenes" are a new concept in Claylib that don't really exist in Raylib. Scenes aim to make it easier to create groups of objects and assets, and allocate your memory up front wherever possible. Using scenes well will also maximize REPL interactivity while you develop your game; more on this in the `do-game-loop` section below. `make-scene` takes two lists -- a list of game **assets** and a list of game **objects**.

**Assets** are where you load your models, textures, audio, etc. from files. You'll use these pre-loaded assets later, as components of this scene's game objects, background music, and so on. At the time of this writing, scene assets are barely implemented and not at all tested... so don't rely on them to do anything useful yet!

**Objects**, in a nutshell, are things that get drawn on the screen. Technically you can put things in here that can't be drawn, but there probably aren't many reasons to, and it could break your draw loop if you aren't careful.

Here, `scene` will contain a single game object that is oh-so-creatively named `text`.

### `make-whatever`
```
,(make-text "Congrats! You created your first window!"
            190
            200
            :size 20
            :color +lightgray+)
```
`make-text` takes three required arguments -- a text string, and the X and Y coordinates where the text should be drawn. In practice you'll frequently pass `:size` and `:color` but they do have sane defaults for the lazy, as does `:spacing`. You can also pass your own `font` object via `:font`... but let's not get ahead of ourselves.

`claylib` exports a number of `make-whatever` functions for these game objects, which you are recommended to use where available. Not every class has such a corresponding function yet.

### `with-scene`
```
(with-scene scene ()
  ...)
```
The `with-scene` macro loads your scene assets if you passed any, and automatically frees your assets _and_ scene objects after the body returns.

"But wait," you say, "what if I don't want that stuff freed yet?" Well, now you know the purpose of that empty list there in the second argument. You can pass a single keyword argument, `:free`, as one of three values: `:now`, `:later`, or `:never`. The default is to free `:now`, where "now" is whenever you close out the current scene. Freeing `:later` pushes those objects to a `*garbage*` list, to be freed whenever you call `(collect-garbage)` (which may be at the end of the _next_ scene unless you change `:free` again). `:never` puts you in the land of manual memory management, for maximum control and minimum sanity.

### `do-game-loop`
```
(do-game-loop (:livesupport t)
  ...)
```
The `do-game-loop` macro is your main loop, where your game will spend most of its time and where performance and consing matter the most. It's just a fancy `do` form, so you can pass corresponding `:vars`, `:end`, and `:result` keywords. The end form has an implied `(or (window-should-close-p) ...)` which means the loop will always terminate when you hit Escape, in addition to whatever conditions you pass.

As for `:livesupport`: `claylib` uses the excellent [livesupport](https://github.com/cbaggers/livesupport) library to provide better interactive development. With it enabled, you can run your game and see your changes at the REPL reflected in real-time. This is where scenes come in, as all you need to do is `setf` a scene object's properties and watch that object be drawn differently. Any functions that are called from your game loop can be similarly live-modified. It is recommended to keep `:livesupport` enabled for development and disable it when you ship your game.

### `with-drawing`
```
(with-drawing ...)
```
A simple macro that encloses the body in a drawing mode, clearing the `*claylib-background*` color at the beginning. Other similar macros:
- `with-2d-mode`
- `with-3d-mode`
- `with-texture-mode`

### `draw-scene-*`
```
(draw-scene-all scene)
```
Draw every game object in the passed scene, _in the order those objects were put into the scene_. For greater specificity, use one of the other functions:
- `draw-scene`
- `draw-scene-except`
- `draw-scene-regex`
