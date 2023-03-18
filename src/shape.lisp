(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass shape (linkable)
    ((%color :initarg :color
             :type rl-color
             :accessor color)
     (%filled :initarg :filled
              :type boolean
              :accessor filled))
    (:default-initargs
     :filled t)))

(child-setter shape color filled)

(define-print-object shape
    (color filled))

(definitializer shape
  :lisp-slots ((%color) (%filled)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass 2d-shape (shape 2d-object)
    ((%color2 :initarg :color2
              :type rl-color
              :accessor color2
              :documentation "The 2nd color to use for a shape drawn with a gradient."))))

(child-setter 2d-shape color2)

(define-print-object 2d-shape
    (color2))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass 3d-shape (shape 3d-object) ()))

(define-print-object 3d-shape
    ())
