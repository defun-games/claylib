# claylib
A Common Lisp 2D/3D game toolkit built on top of [Raylib](https://www.raylib.com/) 4.0.

## Quick Start
Claylib is not yet in Quicklisp. Load claylib.asd and run `(ql:quickload :claylib/examples)` or similar to load all of the available packages. See the next section for what those packages are and how you might use them.

To see the [examples](/examples) in action, just run e.g. `(example-core-01)`. Hit Escape to exit. At the time of this writing, only the [core examples](https://www.raylib.com/examples.html) are done, and a few of those are still missing.

## Packages
This repo contains four separate packages/systems:
- `claylib/wrap` wraps Raylib and Raymath via [cl-autowrap](https://github.com/rpav/cl-autowrap), along with a few small fixes. There's probably no reason to use it directly.
- `claylib/ll` ia a thin layer on top of `claylib/wrap` that adds some convenience features but mostly keeps the C semantics. If you have a lot of experience with Raylib in C or you just really like manual memory management, then `claylib/ll` is for you.
- `claylib` sits atop `claylib/ll` and does a lot of work to try to smooth out and abstract the C semantics away from the user. It's not perfect, nor _can_ it be (more on that later). But its goal is to feel as Lispy as possible, and no Lispier!
- `claylib/examples` contains a number of -- you guessed it -- examples, remixed from [Raylib's own](https://www.raylib.com/examples.html).

## Current Status
`claylib/ll` should be highly usable, as a thin wrapper over Raylib. Only Raylib and Raymath are included, so you will be missing things like 2D physics, but you can always use other libraries for that.

For `claylib`, you're best off reviewing the examples as a survey of what's done and what's not. 2D support is largely complete; 3D support is a bit more iffy. If any piece is particularly important to you, please file an [issue](https://github.com/defun-games/claylib/issues) and we will prioritize it!
