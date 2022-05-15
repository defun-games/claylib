(in-package #:claylib)

(defclass rl-camera-2d ()
  ((%offset :initarg :offset
            :type rl-vector2
            :reader offset)
   (%target :initarg :target
            :type rl-vector2
            :reader target)
   (%c-struct
    :type claylib/ll:camera2d
    :initform (autowrap:alloc 'claylib/ll:camera2d)
    :accessor c-struct)))

(defcreader rot rl-camera-2d rotation camera2d)
(defcreader zoom rl-camera-2d zoom camera2d)

(defcwriter rot rl-camera-2d rotation camera2d number float)
(defcwriter zoom rl-camera-2d zoom camera2d number float)
(defcwriter-struct offset rl-camera-2d offset camera2d vector2 x y)
(defcwriter-struct target rl-camera-2d target camera2d vector2 x y)

(definitializer rl-camera-2d
    (offset rl-vector2) (target rl-vector2) (rot number float) (zoom number float))

(default-free rl-camera-2d)
(default-free-c claylib/ll:camera2d)

(defun make-camera-2d (offset-x offset-y target-x target-y
                       &key (rot 0.0) (zoom 1.0))
  (make-instance 'rl-camera-2d
                 :offset (make-vector2 offset-x offset-y)
                 :target (make-vector2 target-x target-y)
                 :rot rot
                 :zoom zoom))

(defun make-camera-2d-from-vecs (offset target
                                 &key (rot 0.0) (zoom 1.0))
  (make-instance 'rl-camera-2d
                 :offset offset
                 :target target
                 :rot rot
                 :zoom zoom))
