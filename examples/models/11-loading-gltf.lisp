(in-package #:cl-user)
(defpackage claylib/examples/models-11
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/models-11)

(defun pathname-from-relative-path (path)
  "Return a pathname from a path relative to the claylib project root."
  (asdf:system-relative-pathname :claylib path))

(defparameter *scene*
  (make-scene ((raylib-ass (make-model-asset (pathname-from-relative-path
                                         "examples/models/resources/models/gltf/raylib_32x32.glb"))))
              ((model (make-model raylib-ass 0 0 0))
               (grid (make-grid 10 1)))))

(defun main ()
  (with-window (:title "raylib [models] example - loading gltf")
    (let ((camera (make-camera-3d 10 10 10
                                  0 0 0
                                  0 1 0
                                  :mode +camera-free+)))
      (with-scenes *scene*
        (do-game-loop (:livesupport t)
          (update-camera camera)
          (with-drawing
            ;; TODO Why can't I shadow *claylib-background* in the above let instead?
            ;; This is a redundant call to clear-background.
            (clear-background :color +skyblue+)
            (with-3d-mode camera
              (draw-scene-all *scene*))))))))
