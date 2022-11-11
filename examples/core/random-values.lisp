(in-package #:cl-user)
(defpackage claylib/examples/random-values
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/random-values)

(defun main ()
  (with-window (:title "raylib [core] example - generate random values")
    (let* ((rand-value (get-random-value -8 5))
           (frames-counter 0)
           (scene (make-scene ()
                              ((text (make-text "Every 2 seconds a new random value is generated:"
                                                130 100
                                                :size 20 :color +maroon+))
                               (num (make-text (format nil "~d" rand-value)
                                               360 180
                                               :size 80 :color +lightgray+))))))
      (with-scenes scene ()
        (do-game-loop (:livesupport t)
          (incf frames-counter)
          (when (= (mod (/ frames-counter 120) 2) 1)
            (setf rand-value (get-random-value -8 5)
                  (text (scene-object scene 'num)) (format nil "~d" rand-value)
                  frames-counter 0))
          (with-drawing ()
            (draw-scene-all scene)))))))
