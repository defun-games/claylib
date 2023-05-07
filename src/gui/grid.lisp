(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-grid (gui-object text-label)
    ((%spacing :initarg :spacing
               :type number
               :reader spacing)
     (%subdivs :initarg :subdivs
               :type integer
               :accessor subdivs)
     (%position
      :initform (make-vector2 0 0)
      :type rl-vector2
      :reader pos
      :documentation "The GUI-GRID position vector is a libffi return value only. It rarely makes sense to
set this in your own code."))
    (:default-initargs
     :spacing 1.0)
    (:documentation "Grid control, sets POSITION to mouse cell position")))

(define-print-object gui-grid
    (spacing subdivs pos))

(defwriter-float spacing gui-grid)

(child-setter gui-grid subdivs)

(definitializer gui-grid
  :lisp-slots ((%spacing nil float)))

(defun-pt gui-grid claylib/ll:gui-grid
  "Grid control. Allocates a new RL-VECTOR2 unless you pass one."
  (vec rl-vector2 nil (make-vector2 0 0))
  (bounds rl-rectangle)
  (text string)
  (spacing number float)
  (subdivs integer))

(defmethod draw-object ((obj gui-grid))
  (gui-grid (bounds obj)
            (text obj)
            (spacing obj)
            (subdivs obj)
            :vec (pos obj)))

(static-draw draw-gui-grid-object gui-grid)

(defun make-gui-grid (x y width height subdivs &rest args &key text spacing)
  (declare (ignorable text spacing))
  (apply #'make-instance 'gui-grid
         :bounds (make-simple-rec x y width height)
         :subdivs subdivs
         args))
