(in-package #:cl-user)
(defpackage claylib/examples/shapes-9
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/shapes-9)

(defparameter *scene*
  (let ((left-x (- (/ *screen-width* 2) 100))
        (right-x (+ (/ *screen-width* 2) 100))
        (y (/ *screen-height* 2)))
    (make-scene ()
                ((sclera-l (make-circle  left-x y 80 +lightgray+))
                 (sclera-r (make-circle right-x y 80 +lightgray+))
                 (iris-l   (make-circle  left-x y 24 +brown+))
                 (iris-r   (make-circle right-x y 24 +darkgreen+))
                 (pupil-l  (make-circle  left-x y 10 +black+))
                 (pupil-r  (make-circle right-x y 10 +black+))))))

(defun update-eye (iris pupil sclera)
  "Move the IRIS & PUPIL to the mouse position, stopping at the bounds of the SCLERA."
  (setf (pos iris) (get-mouse-position)
        (pos pupil) (get-mouse-position))

  ;; Using claylib/ll because we want a custom circle radius
  (when (= 0 (claylib/ll:check-collision-point-circle (claylib::c-struct (pos iris))
                                                      (claylib::c-struct (pos sclera))
                                                      (float (- (radius sclera) (radius iris)))))
    (let* ((dx (- (x iris) (x sclera)))
           (dy (- (y iris) (y sclera)))
           (angle (atan dy dx))
           (dxx (* (- (radius sclera) (radius iris)) (cos angle)))
           (dyy (* (- (radius sclera) (radius iris)) (sin angle)))
           (x (+ (x sclera) dxx))
           (y (+ (y sclera) dyy)))
      (setf (x iris) x
            (y iris) y
            (x pupil) x
            (y pupil) y))))

(defun main ()
  (with-window (:title "raylib [shapes] example - following eyes")
    (with-scenes *scene*
      (with-scene-objects (sclera-l sclera-r iris-l iris-r pupil-l pupil-r) *scene*
        (do-game-loop (:livesupport t)
          (update-eye iris-l pupil-l sclera-l)
          (update-eye iris-r pupil-r sclera-r)
          (with-drawing ()
            (draw-scene-all *scene*)
            (draw-fps 10 10)))))))
