(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass sphere (3d-shape)
    ((%radius :initarg :radius
              :type (float 0 *)
              :accessor radius)
     (%rings :initarg :rings
             :type (integer 0 *)
             :accessor rings)
     (%slices :initarg :slices
              :type (integer 0 *)
              :accessor slices))
    (:default-initargs
     :rings 16
     :slices 16)))

(child-setter sphere radius rings slices)

(define-print-object sphere
    (radius rings slices))

(definitializer cube
  :lisp-slots ((%radius) (%rings) (%slices)))

(defun make-sphere (x y z radius color &rest args &key filled rings slices)
  (declare (ignorable filled rings slices))
  (apply #'make-instance 'sphere
         :pos (make-vector3 x y z)
         :radius radius
         :color color
         args))

(defun make-sphere-from-vec (pos radius color &rest args &key filled rings slices)
  (declare (ignorable filled rings slices))
  (apply #'make-instance 'sphere
         :pos pos
         :radius radius
         :color color
         args))

(defmethod draw-object ((obj sphere))
  (if (filled obj)
      (claylib/ll:draw-sphere-ex (c-ptr (pos obj))
                                 (radius obj)
                                 (rings obj)
                                 (slices obj)
                                 (c-ptr (color obj)))
      (claylib/ll:draw-sphere-wires (c-ptr (pos obj))
                                    (radius obj)
                                    (rings obj)
                                    (slices obj)
                                    (c-ptr (color obj)))))

(static-draw draw-sphere-object sphere)
