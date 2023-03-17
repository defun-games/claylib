(in-package #:cl-user)
(defpackage claylib/examples/3d-camera-free
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/3d-camera-free)

(defun main ()
  (with-window (:title "raylib [core] example - 3d camera free")
    (let ((camera (make-camera-3d 10 10 10
                                  0 0 0
                                  0 1 0
                                  :fovy 45.0
                                  :projection +camera-perspective+
                                  :mode +camera-free+))
          (scene (make-scene () ((cube (make-cube 0 0 0
                                                  2.0 2.0 2.0
                                                  +red+))
                                 (wires (make-cube 0 0 0
                                                   2.0 2.0 2.0
                                                   +maroon+
                                                   :filled nil))
                                 (grid (make-grid 10 1.0))
                                 (rect (make-rectangle 10 10 320 133 (fade +skyblue+ 0.5 t)))
                                 (rect-border (make-rectangle 10 10 320 133 +blue+ :filled nil))
                                 (t1 (make-text "Free camera default controls:"
                                                20 20
                                                :size 10 :color +black+))
                                 (t2 (make-text "- Mouse Wheel to Zoom in-out"
                                                40 40
                                                :size 10 :color +darkgray+))
                                 (t3 (make-text "- Mouse Wheel Pressed to Pan"
                                                40 60
                                                :size 10 :color +darkgray+))
                                 (t4 (make-text "- Alt + Mouse Wheel Pressed to Rotate"
                                                40 80
                                                :size 10 :color +darkgray+))
                                 (t5 (make-text "- Alt + Ctrl + Mouse Wheel Pressed for Smooth Zoom"
                                                40 100
                                                :size 10 :color +darkgray+))
                                 (t6 (make-text "- Z to zoom to (0, 0, 0)"
                                                40 120
                                                :size 10 :color +darkgray+))))))
      (disable-cursor)
      (with-scenes scene ()
        (do-game-loop (:livesupport t)
          (update-camera camera)
          (when (is-key-down-p +key-z+)
            (setf (x (target camera)) 0
                  (y (target camera)) 0
                  (z (target camera)) 0))
          (with-drawing ()
            (with-3d-mode camera
              (draw-scene scene '(cube wires grid)))
            (draw-scene scene
                        '(rect rect-border
                          t1 t2 t3 t4 t5 t6))))))))
