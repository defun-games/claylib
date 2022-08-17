;;;; claylib.asd

;; Fix SBCL issue where sometimes C floating-point bugs cause Lisp errors or crashes.
;; These bugs appear to originate in Raylib, not Claylib. Why they only show up for some
;; users and not others is currently unknown.
#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-int:set-floating-point-modes :traps nil))

(asdf:defsystem #:claylib/wrap
  :description "Autowrapped Raylib + bug fixes"
  :author "(defun games ()) <hello@defungames.com>"
  :license  "zlib"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-autowrap/libffi)
  :components ((:module "wrap"
                :components
                ((:module "lib"
                  :components ((:static-file "raygui.h")
                               (:static-file "raymath.h")))
                 (:module "spec")
                 (:file "package")
                 (:file "claylib-wrap")))))

(asdf:defsystem #:claylib/ll
  :description "Raylib C semantics with Lispy convenience wrappers"
  :author "(defun games ()) <hello@defungames.com>"
  :license "zlib"
  :version "0.0.1"
  :serial t
  :depends-on (#:claylib/wrap #:cl-plus-c)
  :components ((:module "ll"
                :components
                ((:file "package")
                 (:file "claylib-ll")
                 (:file "validate")))))

(asdf:defsystem #:claylib
  :description "Lispy game toolkit built on top of Raylib"
  :author "(defun games ()) <hello@defungames.com>"
  :license "zlib"
  :version "0.0.1"
  :serial t
  :depends-on (#:claylib/ll #:cl-plus-c #:trivial-garbage #:livesupport #:closer-mop
               #:eager-future2 #:trivial-extensible-sequences)
  :components ((:file "package")
               (:module "src"
                :components ((:file "generic")
                             (:file "helpers")
                             (:file "sequence")
                             (:file "vec")
                             (:file "color")
                             (:file "bounding-box")
                             (:file "game-asset")
                             (:file "game-object")
                             (:file "shape")
                             (:file "circle")
                             (:file "triangle")
                             (:file "pixel")
                             (:file "grid")
                             (:file "matrix")
                             (:file "plane")
                             (:file "line")
                             (:file "rectangle")
                             (:file "polygon")
                             (:file "camera-2d")
                             (:file "camera-3d")
                             (:file "ray")
                             (:file "ray-collision")
                             (:file "image")
                             (:file "texture")
                             (:file "text")
                             (:file "cube")
                             (:file "sphere")
                             (:file "font")
                             (:file "transform")
                             (:file "material")
                             (:file "anim")
                             (:file "mesh")
                             (:file "model")
                             (:file "billboard")
                             (:file "music")
                             (:file "claylib")
                             (:file "scene")
                             (:file "pt-functions")
                             (:file "gui")))))

(asdf:defsystem #:claylib/examples
  :description "Claylib examples, remixed from the original Raylib C versions"
  :author "(defun games ()) <hello@defungames.com>"
  :license "zlib"
  :version "0.0.1"
  :serial t
  :depends-on (#:claylib #:alexandria #:bordeaux-threads)
  :components ((:module "examples"
                :components
                ((:file "package")
                 (:file "helpers")
                 (:module "core"
                  :components
                  ((:file "basic-window")
                   (:file "basic-screen-manager")
                   (:file "input-keys")
                   (:file "input-mouse")
                   (:file "input-mouse-wheel")
                   (:file "2d-camera")
                   (:file "2d-camera-platformer")
                   (:file "3d-camera-mode")
                   (:file "3d-camera-free")
                   (:file "3d-camera-first-person")
                   (:file "3d-picking")
                   (:file "world-screen")
                   (:file "window-letterbox")
                   (:file "window-should-close")
                   (:file "random-values")
                   (:file "loading-thread")
                   (:file "scissor-test")
                   (:file "storage-values")
                   (:file "quat-conversion")
                   (:file "window-flags")
                   (:file "split-screen")
                   (:file "smooth-pixelperfect")))
                 (:module "shapes"
                  :components
                  ((:file "basic-shapes")
                   (:file "bouncing-ball")
                   (:file "colors-palette")
                   (:file "logo-raylib-shapes")
                   (:file "logo-raylib-anim")
                   (:file "rectangle-scaling")
                   (:file "lines-bezier")
                   (:file "collision-area")
                   (:file "following-eyes")))
                 (:module "textures"
                  :components
                  ((:file "logo-raylib-texture")
                   (:file "mouse-painting")
                   (:file "background-scrolling")
                   (:file "sprite-anim")
                   (:file "srcrec-dstrec")
                   (:file "image-drawing")))
                 (:module "gui"
                  :components
                  ((:file "portable-window")
                   (:file "scroll-panel")))
                 (:module "models"
                  :components
                  ((:file "animation")
                   (:file "billboard")
                   (:file "box-collisions")))
                 (:module "text"
                  :components
                  ((:file "raylib-fonts")
                   (:file "font-spritefont")
                   (:file "font-loading")
                   (:file "format-text")
                   (:file "input-box")))
                 (:module "audio"
                  :components
                  ((:file "module-playing")
                   (:file "music-stream")))))))
