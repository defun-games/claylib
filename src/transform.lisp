(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-transform ()
    ((%translation :initarg :trans
                   :type rl-vector3
                   :reader trans)
     (%rotation :initarg :rot
                :type rl-vector4
                :reader rot)
     (%scale :initarg :scale
             :type rl-vector3
             :reader scale)
     (%c-struct
      :type claylib/ll:transform
      :accessor c-struct))
    (:default-initargs
     :c-struct (autowrap:calloc 'claylib/ll:transform))))

(define-print-object rl-transform
    (trans rot scale c-struct))

(defcwriter-struct trans rl-transform translation transform vector3 x y z)
(defcwriter-struct rot rl-transform rotation transform vector4 x y z w)
(defcwriter-struct scale rl-transform scale transform vector3 x y z)

(definitializer rl-transform
  :struct-slots ((%translation) (%rotation) (%scale)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass transform (rl-transform) ()))

(define-print-object transform
    ())


(defconstant +foreign-transform-size+ (autowrap:sizeof 'claylib/ll:transform))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-transforms (rl-sequence)
    ((%cl-array :type (array rl-transform 1)))))

(define-print-object rl-transforms
    ())

(defmethod make-rl-*-array ((c-struct claylib/wrap:transform) num)
  (let ((contents (loop for i below num
                        for trans = (make-instance 'rl-transform)
                        for c-elt = (autowrap:c-aref c-struct i 'claylib/wrap:transform)
                        do (setf (slot-value trans '%c-struct)
                                 c-elt

                                 (slot-value trans '%translation)
                                 (let ((v (make-instance 'rl-vector3)))
                                   (setf (c-struct v) (transform.translation c-elt))
                                   v)

                                 (slot-value trans '%rotation)
                                 (let ((v (make-instance 'rl-vector4)))
                                   (setf (c-struct v) (transform.rotation c-elt))
                                   v)

                                 (slot-value trans '%scale)
                                 (let ((v (make-instance 'rl-vector3)))
                                   (setf (c-struct v) (transform.scale c-elt))
                                   v))
                        collect trans)))
    (make-array num
                :element-type 'rl-transform
                :initial-contents contents)))

(defmethod (setf sequences:elt) (value (sequence rl-transforms) index)
  (check-type value rl-transform)
  (cffi:foreign-funcall "memcpy"
                        :pointer (autowrap:ptr (c-struct (elt sequence index)))
                        :pointer (autowrap:ptr (c-struct value))
                        :int +foreign-transform-size+
                        :void))
