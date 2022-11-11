(in-package #:cl-user)
(defpackage claylib/examples/logo-raylib-shapes
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/logo-raylib-shapes)

(defparameter *scene*
  (make-scene ()
              ((border (make-rectangle (- (/ (get-screen-width) 2) 128)
                                       (- (/ (get-screen-height) 2) 128)
                                       256 256
                                       +black+))
               (fill (make-rectangle (- (/ (get-screen-width) 2) 112)
                                     (- (/ (get-screen-height) 2) 112)
                                     224 224
                                     +raywhite+))
               (logo (make-text "raylib"
                                (- (/ (get-screen-width) 2) 44)
                                (+ (/ (get-screen-height) 2) 48)
                                :size 50
                                :color +black+))
               (desc (make-text "this is NOT a texture!" 350 370 :size 10 :color +gray+)))
              :defer-init t))

(defun main ()
  (with-window (:title "raylib [shapes] example - raylib logo using shapes")
    (with-scenes *scene* ()
      (do-game-loop (:livesupport t)
        (with-drawing ()
          (draw-scene-all *scene*))))))
