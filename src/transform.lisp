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
      :initform (autowrap:calloc 'claylib/ll:transform)
      :accessor c-struct))))

(defcwriter-struct trans rl-transform translation transform vector3 x y z)
(defcwriter-struct rot rl-transform rotation transform vector4 x y z w)
(defcwriter-struct scale rl-transform scale transform vector3 x y z)

(defmethod sync-children ((obj rl-transform))
  (when (typep (c-struct obj) 'autowrap:wrapper)
    (unless (eq (c-struct (trans obj))
                (transform.translation (c-struct obj)))
      (free-later (c-struct (trans obj)))
      (setf (c-struct (trans obj))
            (transform.translation (c-struct obj)))))
  (when (typep (c-struct obj) 'autowrap:wrapper)
    (unless (eq (c-struct (rot obj))
                (transform.rotation (c-struct obj)))
      (free-later (c-struct (rot obj)))
      (setf (c-struct (rot obj))
            (transform.rotation (c-struct obj)))))
  (when (typep (c-struct obj) 'autowrap:wrapper)
    (unless (eq (c-struct (scale obj))
                (transform.scale (c-struct obj)))
      (free-later (c-struct (scale obj)))
      (setf (c-struct (scale obj))
            (transform.scale (c-struct obj))))))

(definitializer rl-transform
  :struct-slots ((%translation) (%rotation) (%scale)))

(default-free rl-transform %translation %rotation %scale)
(default-free-c claylib/ll:transform)



(cffi:defcstruct vector3
  (x :float)
  (y :float)
  (z :float))
(cffi:defcstruct vector4
  (x :float)
  (y :float)
  (z :float)
  (w :float))
(cffi:defcstruct transform
  (translation '(:struct vector3))
  (rotation '(:struct vector4))
  (scale '(:struct vector3)))
(defconstant +foreign-transform-size+ (cffi:foreign-type-size '(:struct transform)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-transforms (sequences:sequence)
    ((%cl-array :type (array rl-transform 1)
                :initarg :cl-array
                :reader cl-array
                :documentation "An RL-TRANSFORM array tracking the C Transform array underneath."))))

(defun make-transform-array (c-struct num)
  "Make an array of rl-model-animation objects using NUM elements of the ModelAnimation wrapper
C-STRUCT.

Warning: this can refer to bogus C data if NUM does not match the real C array length."
  (let ((contents (loop for i below num
                        for trans = (make-instance 'rl-transform)
                        for c-elt = (autowrap:c-aref c-struct i 'claylib/wrap:transform)
                        do (setf (slot-value trans '%c-struct)
                                 c-elt

                                 (slot-value trans '%translation)
                                 (let ((v (make-instance 'rl-vector3)))
                                   (setf (c-struct v (transform.translation))))

                                 (slot-value trans '%rotation)
                                 (let ((v (make-instance 'rl-vector4)))
                                   (setf (c-struct v (transform.rotation))))

                                 (slot-value trans '%scale)
                                 (let ((v (make-instance 'rl-vector3)))
                                   (setf (c-struct v (transform.scale)))))
                        collect trans)))
    (make-array num
                :element-type 'rl-transform
                :initial-contents contents)))

(defmethod sequences:length ((sequence rl-transforms))
  (length (cl-array sequence)))

(defmethod sequences:elt ((sequence rl-transforms) index)
  (elt (cl-array sequence) index))

(defmethod (setf sequences:elt) (value (sequence rl-transforms) index)
  (check-type value rl-transform)
  (cffi:foreign-funcall "memcpy"
                        :pointer (autowrap:ptr (c-struct (elt sequence index)))
                        :pointer (autowrap:ptr (c-struct value))
                        :int +foreign-transform-size+
                        :void))
