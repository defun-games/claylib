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

(defmethod sync-children ((obj rl-bounding-box))
  (unless (eq (c-struct (low obj))
              (bounding-box.min (c-struct obj)))
    (free-later (c-struct (low obj)))
    (setf (c-struct (low obj))
          (bounding-box.min (c-struct obj))))
  (unless (eq (c-struct (high obj))
              (bounding-box.max (c-struct obj)))
    (free-later (c-struct (high obj)))
    (setf (c-struct (high obj))
          (bounding-box.max (c-struct obj)))))

(definitializer rl-bounding-box
  :struct-slots ((%low) (%high)))

(default-free rl-bounding-box %low %high)
(default-free-c claylib/ll:bounding-box)
