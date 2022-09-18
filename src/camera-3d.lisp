(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-camera-3d ()
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
      :initform (autowrap:alloc 'claylib/ll:camera3d)
      :accessor c-struct))))

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

(defmethod sync-children ((obj rl-camera-3d))
  (unless (eq (c-struct (pos obj))
              (camera3d.position (c-struct obj)))
    (free-later (c-struct (pos obj)))
    (setf (c-struct (pos obj))
          (camera3d.position (c-struct obj))))
  (unless (eq (c-struct (target obj))
              (camera3d.target (c-struct obj)))
    (free-later (c-struct (target obj)))
    (setf (c-struct (target obj))
          (camera3d.target (c-struct obj))))
  (unless (eq (c-struct (up obj))
              (camera3d.up (c-struct obj)))
    (free-later (c-struct (up obj)))
    (setf (c-struct (up obj))
          (camera3d.up (c-struct obj)))))

(definitializer rl-camera-3d
  :cname camera3d
  :struct-slots ((%position) (%target) (%up))
  :pt-accessors ((fovy number float 45.0)
                 (projection integer nil +camera-perspective+)))

(default-free rl-camera-3d %position %target %up)
(default-free-c claylib/ll:camera3d)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass camera-3d (rl-camera-3d)
    ((%mode :initarg :mode
            :type integer
            :reader mode))))

(defmethod (setf mode) ((value integer) (camera camera-3d))
  (claylib/ll:set-camera-mode (c-struct camera) value)
  (setf (slot-value camera '%mode) value))

(definitializer camera-3d
  :lisp-slots ((%mode t)))

(default-slot-value camera-3d %mode +camera-custom+)

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
