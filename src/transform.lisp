(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-transform (c-struct)
    ((%translation :initarg :trans
                   :type rl-vector3
                   :reader trans)
     (%rotation :initarg :rot
                :type rl-vector4
                :reader rot)
     (%scale :initarg :scale
             :type rl-vector3
             :reader scale))
    (:default-initargs
     :c-ptr (calloc 'claylib/ll:transform))))

(define-print-object rl-transform
    (trans rot scale c-ptr))

(defcwriter-struct trans rl-transform translation transform vector3 x y z)
(defcwriter-struct rot rl-transform rotation transform vector4 x y z w)
(defcwriter-struct scale rl-transform scale transform vector3 x y z)

(definitializer rl-transform
  :struct-slots ((%translation) (%rotation) (%scale)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass transform (rl-transform) ()))

(define-print-object transform
    ())


(defconstant +foreign-transform-size+ (cffi:foreign-type-size 'claylib/ll:transform))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-transforms (rl-sequence)
    ((%cl-array :type (array rl-transform 1)))))

(define-print-object rl-transforms
    ())

(defun make-rl-transform-array (c-ptr num)
  (let ((contents (loop for i below num
                        for trans = (make-instance 'rl-transform)
                        for c-elt = (cffi:mem-aref c-ptr 'claylib/ll:transform i)
                        do (setf (slot-value trans '%c-ptr)
                                 c-elt

                                 (slot-value trans '%translation)
                                 (let ((v (make-instance 'rl-vector3)))
                                   (setf (c-ptr v) (transform.translation c-elt))
                                   v)

                                 (slot-value trans '%rotation)
                                 (let ((v (make-instance 'rl-vector4)))
                                   (setf (c-ptr v) (transform.rotation c-elt))
                                   v)

                                 (slot-value trans '%scale)
                                 (let ((v (make-instance 'rl-vector3)))
                                   (setf (c-ptr v) (transform.scale c-elt))
                                   v))
                        collect trans)))
    (make-array num
                :element-type 'rl-transform
                :initial-contents contents)))

(defmethod (setf sequences:elt) (value (sequence rl-transforms) index)
  (check-type value rl-transform)
  (cffi:foreign-funcall "memcpy"
                        :pointer (c-ptr (elt sequence index))
                        :pointer (c-ptr value)
                        :int +foreign-transform-size+
                        :void))
