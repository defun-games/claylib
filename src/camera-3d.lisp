(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-camera-3d (c-struct linkable)
    ((%position :initarg :pos
                :type rl-vector3
                :reader pos)
     (%target :initarg :target
              :type rl-vector3
              :reader target)
     (%up :initarg :up
          :type rl-vector3
          :reader up))
    (:default-initargs
     :c-ptr (calloc 'claylib/ll:camera-3d)
     :fovy 45.0
     :projection +camera-perspective+)))

(defreader x rl-camera-3d x pos)
(defreader y rl-camera-3d y pos)
(defreader z rl-camera-3d z pos)
(defcreader fovy rl-camera-3d fovy camera-3d)
(defcreader projection rl-camera-3d projection camera-3d)

(define-print-object rl-camera-3d
    (pos target up fovy projection))

(defwriter x rl-camera-3d x pos number)
(defwriter y rl-camera-3d y pos number)
(defwriter z rl-camera-3d z pos number)
(defcwriter fovy rl-camera-3d fovy camera-3d number float)
(defcwriter projection rl-camera-3d projection camera-3d integer)
(defcwriter-struct pos rl-camera-3d position camera-3d vector3 x y z)
(defcwriter-struct target rl-camera-3d target camera-3d vector3 x y z)
(defcwriter-struct up rl-camera-3d up camera-3d vector3 x y z)

(definitializer rl-camera-3d
  :struct-slots ((%position) (%target) (%up))
  :pt-accessors ((fovy number float)
                 (projection integer)))



(alexandria:define-constant +camera-pro+ -1)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass camera-3d (rl-camera-3d)
    ((%mode :initarg :mode
            :type integer
            :accessor mode)
     (%movement :initarg :movement
                :type rl-vector3
                :accessor movement)
     (%rotation :initarg :rot
                :type rl-vector3
                :accessor rot)
     (%zoom :initarg :zoom
            :type float
            :accessor zoom))
    (:default-initargs
     :mode +camera-custom+
     :movement (make-vector3 0 0 0)
     :rot (make-vector3 0 0 0)
     :zoom 0.0)))

(child-setter camera-3d mode movement rotation zoom)

(define-print-object camera-3d
    (mode movement rot zoom))

(definitializer camera-3d
  :lisp-slots ((%mode) (%movement) (%rotation) (%zoom)))

(defun make-camera-3d (pos-x pos-y pos-z
                       target-x target-y target-z
                       up-x up-y up-z
                       &rest args &key fovy projection mode movement rot zoom)
  (declare (ignorable fovy projection mode movement rot zoom))
  (apply #'make-instance 'camera-3d
         :pos (make-vector3 pos-x pos-y pos-z)
         :target (make-vector3 target-x target-y target-z)
         :up (make-vector3 up-x up-y up-z)
         args))

(defun make-camera-3d-from-vecs (pos target up
                                 &rest args &key fovy projection mode movement rot zoom)
  (declare (ignorable fovy projection mode movement rot zoom))
  (apply #'make-instance 'camera-3d
         :pos pos
         :target target
         :up up
         args))
