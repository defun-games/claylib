(in-package #:claylib)

;; I moved the %position slot out because of the way DEFINITIALIZER is written;
;; it's easier to rely on direct slot info than inherited slot info.
;; This class is probably useless now... emphasis on *probably*.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass game-object () ()))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass 2d-object (game-object)
    ((%position :initarg :pos
                :type rl-vector2
                :accessor pos))))

(defreader x 2d-object x pos)
(defreader y 2d-object y pos)

(defwriter x 2d-object x pos number)
(defwriter y 2d-object y pos number)

(definitializer 2d-object
  :lisp-slots ((%position)))

(default-free 2d-object %position)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass 3d-object (game-object)
    ((%position :initarg :pos
                :type rl-vector3
                :accessor pos)
     (%rot-axis :initarg :rot-axis
                :type rl-vector3
                :accessor rot-axis)
     (%rot-angle :initarg :rot-angle
                 :type number
                 :reader rot-angle))))

(defreader x 3d-object x pos)
(defreader y 3d-object y pos)
(defreader z 3d-object z pos)

(defwriter x 3d-object x pos number)
(defwriter y 3d-object y pos number)
(defwriter z 3d-object z pos number)
(defwriter-float rot-angle 3d-object)

(definitializer 3d-object
  :lisp-slots ((%position)
               (%rot-axis)
               (%rot-angle t)))

(default-slot-value 3d-object %rot-axis (make-vector3 0 0 0))
(default-slot-value 3d-object %rot-angle 0.0)

(default-free 3d-object %position %rot-axis)
