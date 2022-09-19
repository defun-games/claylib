(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
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
      :accessor c-struct))))

(defcreader rot rl-camera-2d rotation camera2d)
(defcreader zoom rl-camera-2d zoom camera2d)

(defcwriter rot rl-camera-2d rotation camera2d number float)
(defcwriter zoom rl-camera-2d zoom camera2d number float)
(defcwriter-struct offset rl-camera-2d offset camera2d vector2 x y)
(defcwriter-struct target rl-camera-2d target camera2d vector2 x y)

(defmethod sync-children ((obj rl-camera-2d))
  (unless (eq (c-struct (offset obj))
              (camera2d.offset (c-struct obj)))
    (free-later (c-struct (offset obj)))
    (setf (c-struct (offset obj))
          (camera2d.offset (c-struct obj))))
  (unless (eq (c-struct (target obj))
              (camera2d.target (c-struct obj)))
    (free-later (c-struct (target obj)))
    (setf (c-struct (target obj))
          (camera2d.target (c-struct obj)))))

(definitializer rl-camera-2d
  :struct-slots ((%offset) (%target))
  :pt-accessors ((rot number float 0.0)
                 (zoom number float 1.0)))

(default-free rl-camera-2d %offset %target)
(default-free-c claylib/ll:camera2d)

(defun make-camera-2d (offset-x offset-y target-x target-y
                       &rest args &key rot zoom)
  (declare (ignorable rot zoom))
  (apply #'make-instance 'rl-camera-2d
         :offset (make-vector2 offset-x offset-y)
         :target (make-vector2 target-x target-y)
         args))

(defun make-camera-2d-from-vecs (offset target
                                 &rest args &key rot zoom)
  (declare (ignorable rot zoom))
  (apply #'make-instance 'rl-camera-2d
         :offset offset
         :target target
         args))
