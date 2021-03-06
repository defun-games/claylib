(in-package #:cl-user)
(defpackage claylib/examples/core-1
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/core-1)


(defparameter *scene* (make-scene ()
                                  ((text (make-text "Congrats! You created your first window!"
                                                    190
                                                    200
                                                    :size 20
                                                    :color +lightgray+)))))

(defun main ()
  (with-window (:title "raylib [core] example - basic window")
    (with-scenes *scene*
      (do-game-loop (:livesupport t)
        (with-drawing
          (draw-scene-all *scene*))))))
