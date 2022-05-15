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



(defclass 2d-shape (shape 2d-object)
  ((%color2 :initarg :color2
            :initform nil
            :type (or rl-color null)
            :accessor color2)))

(defclass 3d-shape (shape 3d-object) ())
