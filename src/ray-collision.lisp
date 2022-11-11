(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-ray-collision ()
    ((%point :initarg :point
             :type rl-vector3
             :reader point)
     (%normal :initarg :normal
              :type rl-vector3
              :reader normal)
     (%c-struct
      :type claylib/ll:ray-collision
      :accessor c-struct))
    (:default-initargs
     :c-struct (autowrap:calloc 'claylib/ll:ray-collision))))

(defcreader-bool hit rl-ray-collision hit ray-collision)
(defcreader distance rl-ray-collision distance ray-collision)

(defcwriter-bool hit rl-ray-collision hit ray-collision)
(defcwriter distance rl-ray-collision distance ray-collision number float)
(defcwriter-struct point rl-ray-collision point ray-collision vector3 x y z)
(defcwriter-struct normal rl-ray-collision normal ray-collision vector3 x y z)

(definitializer rl-ray-collision
  :struct-slots ((%point) (%normal))
  :pt-accessors ((hit boolean)
                 (distance number float)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass ray-collision (rl-ray-collision) ()))

(defun make-ray-collision (point-x point-y point-z normal-x normal-y normal-z)
  (make-instance 'ray-collision
                 :point (make-vector3 point-x point-y point-z)
                 :normal (make-vector3 normal-x normal-y normal-z)))

(defun make-ray-collision-from-vecs (point normal)
  (make-instance 'ray-collision
                 :point point
                 :normal normal))
