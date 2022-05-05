(in-package #:claylib/examples)

(defun colorize (clr)
  "Determines current color based on mouse actions."
  (cond
    ((is-mouse-button-pressed-p +mouse-button-left+)    +maroon+)
    ((is-mouse-button-pressed-p +mouse-button-right+)   +darkblue+)
    ((is-mouse-button-pressed-p +mouse-button-middle+)  +lime+)
    (t clr)))

(defun example-core-04 ()
  (with-window (:title "raylib [core] example - mouse input")
    (let ((scene (make-scene ()
                             `((text ,(make-text "" 10 10 :size 20 :color +darkgray+))
                               (ball ,(make-circle -100 -100 40 +darkblue+))))))
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
