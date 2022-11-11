(in-package #:cl-user)
(defpackage claylib/examples/input-mouse
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/input-mouse)


(defun colorize (clr)
  "Determines current color based on mouse actions."
  (cond
    ((is-mouse-button-pressed-p +mouse-button-left+)    +maroon+)
    ((is-mouse-button-pressed-p +mouse-button-middle+)  +lime+)
    ((is-mouse-button-pressed-p +mouse-button-right+)   +darkblue+)
    ((is-mouse-button-pressed-p +mouse-button-side+)    +purple+)
    ((is-mouse-button-pressed-p +mouse-button-extra+)   +yellow+)
    ((is-mouse-button-pressed-p +mouse-button-forward+) +orange+)
    ((is-mouse-button-pressed-p +mouse-button-back+)    +beige+)
    (t clr)))

(defun main ()
  (with-window (:title "raylib [core] example - mouse input")
    (let ((scene (make-scene ()
                             ((text (make-text "move ball with mouse and click mouse button to change color"
                                               10 10
                                               :size 20 :color +darkgray+))
                              (ball (make-circle -100 -100 40 +darkblue+))))))
      (with-scenes scene ()
        (do-game-loop (:livesupport t
                       :vars ((color +darkblue+)))
          (with-scene-objects (ball text) scene
            (setf (x ball) (get-mouse-x)
                  (y ball) (get-mouse-y)
                  color (colorize color)
                  (color ball) color)
            (with-drawing ()
              (draw-scene-all scene))))))))
