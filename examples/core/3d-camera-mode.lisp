(in-package #:cl-user)
(defpackage claylib/examples/3d-camera-mode
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/3d-camera-mode)

(defun main ()
  (with-window (:title "raylib [core] example - 3d camera mode")
    (let ((camera (make-camera-3d 0 10 10
                                  0 0 0
                                  0 1 0))
          (scene (make-scene () ((cube (make-cube 0 0 0
                                                  2.0 2.0 2.0
                                                  +red+))
                                 (wires (make-cube 0 0 0
                                                   2.0 2.0 2.0
                                                   +maroon+
                                                   :filled nil))
                                 (grid (make-grid 10 1.0))
                                 (text (make-text "Welcome to the third dimension!"
                                                  10 40
                                                  :size 20 :color +darkgray+))))))
      (with-scenes scene ()
        (do-game-loop (:livesupport t)
          (with-drawing ()
            (with-3d-mode camera
              (draw-scene-except scene 'text))
            (draw-scene scene 'text)
            (draw-fps 10 10)))))))
