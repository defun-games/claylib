(in-package #:cl-user)
(defpackage claylib/examples/input-mouse-wheel
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/input-mouse-wheel)

(defun main ()
  (with-window (:title "raylib [core] example - input mouse wheel")
    (let ((scene
            (make-scene ()
                        ((box (make-rectangle (- (/ *screen-width* 2.0) 40)
                                              (- (/ *screen-height* 2.0) 40)
                                              80.0
                                              80.0
                                              +maroon+))
                         (text (make-text "Use mouse wheel to move the cube up and down!"
                                          10 10
                                          :size 20 :color +gray+))
                         (subtext (make-text "" 10 40 :size 20 :color +lightgray+))))))
      (with-scenes scene ()
        (do-game-loop (:livesupport t)
          (with-scene-objects (box subtext) scene
            (decf (y box) (* (get-mouse-wheel-move) 4))
            (setf (text subtext) (format nil "Box position Y: ~$" (y box))))
          (with-drawing ()
            (draw-scene-all scene)))))))
