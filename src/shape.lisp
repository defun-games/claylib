(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass shape (game-object)
    ((%color :initarg :color
             :type rl-color
             :accessor color)
     (%filled :initarg :filled
              :type boolean
              :accessor filled))))

(definitializer shape
  :lisp-slots ((%color) (%filled)))

(default-slot-value shape %filled t)

;; TODO: free colors?



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass 2d-shape (shape 2d-object)
    ((%color2 :initarg :color2
              :type rl-color
              :accessor color2
              :documentation "The 2nd color to use for a shape drawn with a gradient."))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass 3d-shape (shape 3d-object) ()))
