;;;; claylib.asd

(asdf:defsystem #:claylib/wrap
  :description "Autowrapped Raylib + bug fixes"
  :author "(defun games ()) <hello@defungames.com>"
  :license  "TODO"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-autowrap/libffi)
  :components ((:module "wrap"
                :components
                ((:module "lib"
                  :components ((:static-file "raylib.h")
                               (:static-file "raymath.h")))
                 (:module "spec")
                 (:file "package")
                 (:file "claylib-wrap")))))

(asdf:defsystem #:claylib/ll
  :description "Raylib C semantics with Lispy convenience wrappers"
  :author "(defun games ()) <hello@defungames.com>"
  :license "TODO"
  :version "0.0.1"
  :serial t
  :depends-on (#:claylib/wrap #:cl-plus-c)
  :components ((:module "ll"
                :components
                ((:file "package")
                 (:file "claylib-ll")))))

(asdf:defsystem #:claylib
  :description "Lispy game toolkit built on top of Raylib"
  :author "(defun games ()) <hello@defungames.com>"
  :license "TODO"
  :version "0.0.1"
  :serial t
  :depends-on (#:claylib/ll #:cl-plus-c #:trivial-garbage #:livesupport #:closer-mop)
  :components ((:file "package")
               (:module "src"
                :components ((:file "generic")
                             (:file "helpers")
                             (:file "vec")
                             (:file "color")
                             (:file "bounding-box")
                             (:file "game-asset")
                             (:file "game-object")
                             (:file "shape")
                             (:file "circle")
                             (:file "triangle")
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
                             (:file "font")
                             (:file "transform")
                             (:file "material")
                             (:file "anim")
                             (:file "mesh")
                             (:file "model")
                             (:file "claylib")
                             (:file "scene")
                             (:file "pt-functions")))))

(asdf:defsystem #:claylib/examples
  :description "Claylib examples, remixed from the original Raylib C versions"
  :author "(defun games ()) <hello@defungames.com>"
  :license "TODO"
  :version "0.0.1"
  :serial t
  :depends-on (#:claylib)
  :components ((:module "examples"
                :components
                ((:file "package")
                 (:module "core"
                  :components
                  ((:file "01-window")
                   (:file "02-manager")
                   (:file "03-keyboard")
                   (:file "04-mouse")
                   (:file "05-mouse-wheel")
                   (:file "09-2d-camera")
                   (:file "10-2d-camera-platformer")
                   (:file "11-3d-camera")
                   (:file "12-3d-camera-free")
                   (:file "13-3d-camera-first-person")
                   (:file "14-3d-picking")
                   (:file "15-world-screen")
                   (:file "17-window-letterbox")
                   (:file "19-random-values")
                   (:file "20-scissor-test")
                   (:file "21-storage-values")
                   (:file "24-quat-conversion")
                   (:file "25-window-flags")
                   (:file "26-split-screen")
                   (:file "27-smooth-pixelperfect")
                   (:file "28-custom-frame-control")))
                 (:module "shapes"
                  :components
                  ((:file "01-basic-shapes")
                   (:file "02-bouncing-ball")
                   (:file "03-colors-palette")))
                 (:module "textures"
                  :components
                  ((:file "01-logo-raylib")
                   (:file "02-mouse-painting")
                   (:file "03-rectangle")
                   (:file "04-srcrec-dstrec")
                   (:file "05-image-drawing")))))))
