(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass grid (linkable)
    ((%slices :initarg :slices
              :type integer
              :accessor slices)
     (%spacing :initarg :spacing
               :type number
               :reader spacing))))

(define-print-object grid
    (slices spacing))

(defwriter-float spacing grid)

(child-setter grid slices)

(definitializer grid
  :lisp-slots ((%slices)
               (%spacing t)))

(defun make-grid (slices spacing)
  (make-instance 'grid :slices slices :spacing spacing))

(defmethod draw-object ((obj grid))
  (claylib/ll:draw-grid (slices obj) (spacing obj)))

(static-draw draw-grid-object grid)
