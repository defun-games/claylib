(in-package #:cl-user)
(defpackage claylib/examples/following-eyes
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/following-eyes)

(defparameter *scene*
  (let ((left-x (- (/ *screen-width* 2) 100))
        (right-x (+ (/ *screen-width* 2) 100))
        (y (/ *screen-height* 2))
        (s-rad 80)
        (i-rad 24)
        (p-rad 10))
    (make-scene ()
                ((sclera-l (make-circle  left-x y s-rad +lightgray+))
                 (sclera-r (make-circle right-x y s-rad +lightgray+))
                 (iris-l   (make-circle  left-x y i-rad +brown+))
                 (iris-r   (make-circle right-x y i-rad +darkgreen+))
                 (pupil-l  (make-circle  left-x y p-rad +black+))
                 (pupil-r  (make-circle right-x y p-rad +black+))
                 (collision-circle (make-instance 'circle :radius (- s-rad i-rad)))))))

(defun update-eye (iris pupil sclera collision-circle)
  "Move the IRIS & PUPIL to the mouse position, stopping at the bounds of the SCLERA."
  (setf (pos iris) (get-mouse-position)
        (pos pupil) (get-mouse-position)
        (pos collision-circle) (pos sclera))

  (unless (check-collision-point-circle (pos iris) collision-circle)
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
    (with-scenes *scene* ()
      (with-scene-objects (sclera-l sclera-r iris-l iris-r pupil-l pupil-r collision-circle) *scene*
        (do-game-loop (:livesupport t)
          (update-eye iris-l pupil-l sclera-l collision-circle)
          (update-eye iris-r pupil-r sclera-r collision-circle)
          (with-drawing ()
            (draw-scene-except *scene* 'collision-circle)
            (draw-fps 10 10)))))))
