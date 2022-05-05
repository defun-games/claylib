(in-package #:claylib)

(defclass grid ()
  ((%slices :initarg :slices
            :type integer
            :accessor slices)
   (%spacing :initarg :spacing
             :type (or integer float)
             :reader spacing)))

(defwriter-float spacing grid)

(definitializer-float grid spacing)

(default-free grid)

(defun make-grid (slices spacing)
  (make-instance 'grid :slices slices :spacing spacing))

(defmethod draw-object ((obj grid))
  (claylib/ll:draw-grid (slices obj) (spacing obj)))
