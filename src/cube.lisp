(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass cube (3d-shape)
    ((%size :initarg :size
            :type rl-vector3
            :accessor size))))

(defreader width cube x size)
(defreader height cube y size)
(defreader len cube z size)

(define-print-object cube
    (size))

(child-setter cube size)

(defwriter width cube x size number)
(defwriter height cube y size number)
(defwriter len cube z size number)

(definitializer cube
  :lisp-slots ((%size)))

(defun make-cube (x y z width height length color &rest args &key filled)
  (declare (ignorable filled))
  (apply #'make-instance 'cube
         :pos (make-vector3 x y z)
         :size (make-vector3 width height length)
         :color color
         args))

(defun make-cube-from-vecs (pos size color &rest args &key filled)
  (declare (ignorable filled))
  (apply #'make-instance 'cube
         :pos pos
         :size size
         :color color
         args))

(defmethod draw-object ((obj cube))
  (funcall (if (filled obj) #'claylib/ll:draw-cube-v #'claylib/ll:draw-cube-wires-v)
           (c-ptr (pos obj))
           (c-ptr (size obj))
           (c-ptr (color obj))))

(static-draw draw-cube-object cube)
