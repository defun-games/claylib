(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-camera-2d (linkable)
    ((%offset :initarg :offset
              :type rl-vector2
              :reader offset)
     (%target :initarg :target
              :type rl-vector2
              :reader target)
     (%c-struct
      :type claylib/ll:camera2d
      :accessor c-struct))
    (:default-initargs
     :c-struct (autowrap:calloc 'claylib/ll:camera2d)
     :rot 0.0
     :zoom 1.0)))

(defcreader rot rl-camera-2d rotation camera2d)
(defcreader zoom rl-camera-2d zoom camera2d)

(define-print-object rl-camera-2d
    (offset target rot zoom))

(defcwriter rot rl-camera-2d rotation camera2d number float)
(defcwriter zoom rl-camera-2d zoom camera2d number float)
(defcwriter-struct offset rl-camera-2d offset camera2d vector2 x y)
(defcwriter-struct target rl-camera-2d target camera2d vector2 x y)

(definitializer rl-camera-2d
  :struct-slots ((%offset) (%target))
  :pt-accessors ((rot number float)
                 (zoom number float)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass camera-2d (rl-camera-2d) ()))

(define-print-object camera-2d
    ())

(defun make-camera-2d (offset-x offset-y target-x target-y
                       &rest args &key rot zoom)
  (declare (ignorable rot zoom))
  (apply #'make-instance 'camera-2d
         :offset (make-vector2 offset-x offset-y)
         :target (make-vector2 target-x target-y)
         args))

(defun make-camera-2d-from-vecs (offset target
                                 &rest args &key rot zoom)
  (declare (ignorable rot zoom))
  (apply #'make-instance 'camera-2d
         :offset offset
         :target target
         args))
