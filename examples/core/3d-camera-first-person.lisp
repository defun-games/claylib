(in-package #:cl-user)
(defpackage claylib/examples/3d-camera-first-person
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/3d-camera-first-person)

(defun main ()
  (with-window (:title "raylib [core] example - 3d camera first person")
    (let ((camera (make-camera-3d 4 2 4
                                  0 1.8 0
                                  0 1 0
                                  :fovy 60.0
                                  :projection +camera-perspective+
                                  :mode +camera-first-person+))
          (scene (make-scene ()
                             ((ground (make-plane 0 0 0 32 32 +lightgray+))
                              (blue (make-cube -16 2.5 0
                                               1 5 32
                                               +blue+))
                              (green (make-cube 16 2.5 0
                                                1 5 32
                                                +lime+))
                              (yellow (make-cube 0 2.5 16
                                                 32 5 1
                                                 +gold+))
                              (rect (make-rectangle 10 10 220 70 (fade +skyblue+ 0.5 t)))
                              (rect-border (make-rectangle 10 10 220 70 +blue+ :filled nil))
                              (text1 (make-text "First person camera default controls:"
                                                20 20
                                                :size 10 :color +black+))
                              (text2 (make-text "- Move with keys: W, A, S, D"
                                                40 40
                                                :size 10 :color +darkgray+))
                              (text3 (make-text "- Mouse move to look around"
                                                40 60
                                                :size 10 :color +darkgray+))))))
      (dotimes (i 20)
        (let* ((h (get-random-value 1 12))
               (pos (make-vector3 (get-random-value -15 15)
                                  (/ h 2.0)
                                  (get-random-value -15 15)))
               (color (make-color (get-random-value 20 255)
                                  (get-random-value 10 55)
                                  30))
               (size (make-vector3 2.0 h 2.0)))
          (setf (gethash (gensym "COLUMN") (objects scene))
                (make-cube-from-vecs pos size color)
                (gethash (gensym "COLUMN") (objects scene))
                (make-cube-from-vecs pos size +maroon+ :filled nil))))
      (with-scenes scene ()
        (do-game-loop (:livesupport t)
          (update-camera camera)
          (with-drawing ()
            (with-3d-mode camera
              (draw-scene scene 'ground 'blue 'green 'yellow)
              (draw-scene-regex scene "^COLUMN"))
            (draw-scene scene 'rect 'rect-border 'text1 'text2 'text3)))))))
