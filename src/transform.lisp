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
      :initform (autowrap:alloc 'claylib/ll:transform)
      :accessor c-struct))))

(defcwriter-struct trans rl-transform translation transform vector3 x y z)
(defcwriter-struct rot rl-transform rotation transform vector4 x y z w)
(defcwriter-struct scale rl-transform scale transform vector3 x y z)

(definitializer rl-transform
  :struct-slots ((%translation) (%rotation) (%scale)))

(default-free rl-transform)
(default-free-c claylib/ll:transform)
