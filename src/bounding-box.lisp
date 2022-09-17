(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-bounding-box ()
    ((%low :initarg :low
           :type rl-vector3
           :reader low)
     (%high :initarg :high
            :type rl-vector3
            :reader high)
     (%c-struct
      :type claylib/ll:bounding-box
      :initform (autowrap:alloc 'claylib/ll:bounding-box)
      :accessor c-struct))))

(defcwriter-struct low rl-bounding-box min bounding-box vector3 x y z)
(defcwriter-struct high rl-bounding-box max bounding-box vector3 x y z)

(definitializer rl-bounding-box
  :struct-slots ((%low) (%high)))

(default-free rl-bounding-box)
(default-free-c claylib/ll:bounding-box)
