(in-package #:claylib)

(defclass shape (game-object)
  ((%color :initarg :color
           :type rl-color
           :accessor color)
   (%filled :initarg :filled
            :type boolean
            :accessor filled)))

(definitializer shape (color rl-color nil) (filled boolean nil t))

(default-slot-value shape %filled t)

;; TODO: free colors?



(defclass 2d-shape (shape 2d-object) ())
(defclass 3d-shape (shape 3d-object) ())
