(in-package #:cl-user)
(defpackage claylib/examples/input-keys
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/input-keys)

(defmacro reposition (coord ball crsr delta)
  "Repositions X coordinate if the specified cursor key is pressed."
  ;; NOTE: If this macro seems unhygienic, remember that setf
  ;;       doesn't evaluate its first argument.
  `(when (is-key-down-p ,crsr)
     (incf (,coord ,ball) ,delta)))


(defun move-ball (ball)
  "Repositions the ball on the screen based on state of the cursor keys."
  (reposition x ball +key-left+  -2.0)
  (reposition x ball +key-right+ +2.0)
  (reposition y ball +key-up+    -2.0)
  (reposition y ball +key-down+  +2.0))

(defun main ()
  (with-window (:title "raylib [core] example - keyboard input")
    (let ((scene (make-scene () ((text (make-text "Move the ball with arrow keys"
                                                  10 10
                                                  :size 20 :color +darkgray+))
                                 (ball (make-circle (/ *screen-width* 2.0)
                                                    (/ *screen-height* 2.0)
                                                    50.0
                                                    +maroon+))))))
      (with-scenes scene ()
        (do-game-loop (:livesupport t)
          (move-ball (scene-object scene 'ball))
          (with-drawing ()
            (draw-scene-all scene)))))))
