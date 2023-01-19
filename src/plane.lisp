(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass plane (3d-shape)
    ((%height :initform 0.0)
     (%size :initarg :size
            :type rl-vector2
            :accessor size))))

(defreader width plane x size)
(defreader len plane y size)

(define-print-object plane
    (size width len))

(defwriter width plane x size number)
(defwriter len plane y size number)

(definitializer plane
  :lisp-slots ((%size)))

(defun make-plane (x y z width length color)
  (make-instance 'plane
                 :pos (make-vector3 x y z)
                 :size (make-vector2 width length)
                 :color color))

(defun make-plane-from-vecs (center size color)
  (make-instance 'plane
                 :pos center
                 :size size
                 :color color))

(defmethod draw-object ((obj plane))
  (claylib/ll:draw-plane (c-struct (pos obj))
                         (c-struct (size obj))
                         (c-struct (color obj))))
