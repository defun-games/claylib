(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-camera-3d (linkable)
    ((%position :initarg :pos
                :type rl-vector3
                :reader pos)
     (%target :initarg :target
              :type rl-vector3
              :reader target)
     (%up :initarg :up
          :type rl-vector3
          :reader up)
     (%c-struct
      :type claylib/ll:camera3d
      :accessor c-struct))
    (:default-initargs
     :c-struct (autowrap:calloc 'claylib/ll:camera3d)
     :fovy 45.0
     :projection +camera-perspective+)))

(defreader x rl-camera-3d x pos)
(defreader y rl-camera-3d y pos)
(defreader z rl-camera-3d z pos)
(defcreader fovy rl-camera-3d fovy camera3d)
(defcreader projection rl-camera-3d projection camera3d)

(defwriter x rl-camera-3d x pos number)
(defwriter y rl-camera-3d y pos number)
(defwriter z rl-camera-3d z pos number)
(defcwriter fovy rl-camera-3d fovy camera3d number float)
(defcwriter projection rl-camera-3d projection camera3d integer)
(defcwriter-struct pos rl-camera-3d position camera3d vector3 x y z)
(defcwriter-struct target rl-camera-3d target camera3d vector3 x y z)
(defcwriter-struct up rl-camera-3d up camera3d vector3 x y z)

(definitializer rl-camera-3d
  :struct-slots ((%position) (%target) (%up))
  :pt-accessors ((fovy number float)
                 (projection integer)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass camera-3d (rl-camera-3d)
    ((%mode :initarg :mode
            :type integer
            :reader mode))
    (:default-initargs
     :mode +camera-custom+)))

(defmethod (setf mode) ((value integer) (camera camera-3d))
  (claylib/ll:set-camera-mode (c-struct camera) value)
  (setf (slot-value camera '%mode) value))

(definitializer camera-3d
  :lisp-slots ((%mode t)))

(defun make-camera-3d (pos-x pos-y pos-z
                       target-x target-y target-z
                       up-x up-y up-z
                       &rest args &key fovy projection mode)
  (declare (ignorable fovy projection mode))
  (apply #'make-instance 'camera-3d
         :pos (make-vector3 pos-x pos-y pos-z)
         :target (make-vector3 target-x target-y target-z)
         :up (make-vector3 up-x up-y up-z)
         args))

(defun make-camera-3d-from-vecs (pos target up
                                 &rest args &key fovy projection mode)
  (declare (ignorable fovy projection mode))
  (apply #'make-instance 'camera-3d
         :pos pos
         :target target
         :up up
         args))
