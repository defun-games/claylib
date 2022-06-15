(in-package #:cl-user)
(defpackage claylib/examples/core-4
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/core-4)


(defun colorize (clr)
  "Determines current color based on mouse actions."
  (cond
    ((is-mouse-button-pressed-p +mouse-button-left+)    +maroon+)
    ((is-mouse-button-pressed-p +mouse-button-right+)   +darkblue+)
    ((is-mouse-button-pressed-p +mouse-button-middle+)  +lime+)
    (t clr)))

(defun main ()
  (with-window (:title "raylib [core] example - mouse input")
    (let ((scene (make-scene ()
                             ((text (make-text "" 10 10 :size 20 :color +darkgray+))
                              (ball (make-circle -100 -100 40 +darkblue+))))))
      (with-scene scene ()
        (do-game-loop (:livesupport t
                       :vars ((color +darkblue+)))
          (with-scene-objects (ball text) scene
            (setf (x ball) (get-mouse-x)
                  (y ball) (get-mouse-y)
                  (text text) (format nil "[~a,~a]" (x ball) (y ball))
                  color (colorize color)
                  (color ball) color)
            (with-drawing
              (draw-scene-all scene))))))))
