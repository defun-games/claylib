(in-package #:claylib)

(defclass rl-ray-collision ()
  ((%point :initarg :point
           :type rl-vector3
           :reader point)
   (%normal :initarg :normal
            :type rl-vector3
            :reader normal)
   (%c-struct
    :type claylib/ll:ray-collision
    :initform (autowrap:alloc 'claylib/ll:ray-collision)
    :accessor c-struct)))

(defcreader-bool hit rl-ray-collision hit ray-collision)
(defcreader distance rl-ray-collision distance ray-collision)

(defcwriter-bool hit rl-ray-collision hit ray-collision)
(defcwriter distance rl-ray-collision distance ray-collision number float)
(defcwriter-struct point rl-ray-collision point ray-collision vector3 x y z)
(defcwriter-struct normal rl-ray-collision normal ray-collision vector3 x y z)

(definitializer rl-ray-collision
    (hit boolean nil nil) (distance number float 0.0) (point rl-vector3) (normal rl-vector3))

(default-free rl-ray-collision)
(default-free-c claylib/ll:ray-collision)

(defun make-ray-collision (point-x point-y point-z normal-x normal-y normal-z)
  (make-instance 'rl-ray-collision
                 :point (make-vector3 point-x point-y point-z)
                 :normal (make-vector3 normal-x normal-y normal-z)))

(defun make-ray-collision-from-vecs (point normal)
  (make-instance 'rl-ray-collision
                 :point point
                 :normal normal))

(defun get-ray-collision-box (ray box &optional rc)
  "Gets a collision box for the passed ray and bounding box.
Allocates a new RAY-COLLISION unless you pass one."
  (check-type ray ray)
  (check-type box rl-bounding-box)
  (check-type rc (or null rl-ray-collision))
  (let ((rc (or rc (make-ray-collision 0 0 0 0 0 0))))
    (claylib/ll:get-ray-collision-box (c-struct rc)
                                      (c-struct ray)
                                      (c-struct box))
    rc))
