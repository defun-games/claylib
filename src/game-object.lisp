(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass 2d-object (linkable)
    ((%position :initarg :pos
                :type rl-vector2
                :accessor pos))))

(defreader x 2d-object x pos)
(defreader y 2d-object y pos)

(define-print-object 2d-object
    (pos))

(child-setter 2d-object pos)

(defwriter x 2d-object x pos number)
(defwriter y 2d-object y pos number)

(definitializer 2d-object
  :lisp-slots ((%position)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass 3d-object (linkable)
    ((%position :initarg :pos
                :type rl-vector3
                :accessor pos)
     (%rot-axis :initarg :rot-axis
                :type rl-vector3
                :accessor rot-axis)
     (%rot-angle :initarg :rot-angle
                 :type number
                 :reader rot-angle))
    (:default-initargs
     :rot-axis (make-vector3 0 0 0)
     :rot-angle 0.0)))

(defreader x 3d-object x pos)
(defreader y 3d-object y pos)
(defreader z 3d-object z pos)

(define-print-object 3d-object
    (pos rot-axis rot-angle))

(child-setter 3d-object pos rot-axis)

(defwriter x 3d-object x pos number)
(defwriter y 3d-object y pos number)
(defwriter z 3d-object z pos number)
(defwriter-float rot-angle 3d-object)

(definitializer 3d-object
  :lisp-slots ((%position)
               (%rot-axis)
               (%rot-angle t)))
