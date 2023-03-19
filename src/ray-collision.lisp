(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-ray-collision (c-struct linkable)
    ((%point :initarg :point
             :type rl-vector3
             :reader point)
     (%normal :initarg :normal
              :type rl-vector3
              :reader normal))
    (:default-initargs
     :c-ptr (calloc 'claylib/ll:ray-collision))))

(defcreader hit rl-ray-collision hit ray-collision)
(defcreader distance rl-ray-collision distance ray-collision)

(define-print-object rl-ray-collision
    (point normal hit distance))

(defcwriter hit rl-ray-collision hit ray-collision)
(defcwriter distance rl-ray-collision distance ray-collision number float)
(defcwriter-struct point rl-ray-collision point ray-collision vector3 x y z)
(defcwriter-struct normal rl-ray-collision normal ray-collision vector3 x y z)

(definitializer rl-ray-collision
  :struct-slots ((%point) (%normal))
  :pt-accessors ((hit boolean)
                 (distance number float)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass ray-collision (rl-ray-collision) ()))

(define-print-object ray-collision
    ())

(defun make-ray-collision (point-x point-y point-z normal-x normal-y normal-z)
  (make-instance 'ray-collision
                 :point (make-vector3 point-x point-y point-z)
                 :normal (make-vector3 normal-x normal-y normal-z)))

(defun make-ray-collision-from-vecs (point normal)
  (make-instance 'ray-collision
                 :point point
                 :normal normal))
