(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-transform (c-struct linkable)
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

(defun make-rl-transform-array (c-ptr num &optional finalize)
  (let ((contents (loop for i below num
                        for c-elt = (cffi:mem-aref c-ptr 'claylib/ll:transform i)
                        for trans = (make-instance 'rl-transform :c-ptr c-elt
                                                                 :finalize (when finalize (= i 0)))
                        do (setf (slot-value trans '%translation)
                                 (make-instance 'rl-vector3
                                                :c-ptr (field-value c-elt 'transform 'translation)
                                                :finalize nil)

                                 (slot-value trans '%rotation)
                                 (make-instance 'rl-vector4
                                                :c-ptr (field-value c-elt 'transform 'rotation)
                                                :finalize nil)

                                 (slot-value trans '%scale)
                                 (make-instance 'rl-vector3
                                                :c-ptr (field-value c-elt 'transform 'scale)
                                                :finalize nil))
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
