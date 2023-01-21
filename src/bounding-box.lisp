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
      :accessor c-struct))
    (:default-initargs
     :c-struct (autowrap:calloc 'claylib/ll:bounding-box))))

(define-print-object rl-bounding-box
    (low high))

(defcwriter-struct low rl-bounding-box min bounding-box vector3 x y z)
(defcwriter-struct high rl-bounding-box max bounding-box vector3 x y z)

(definitializer rl-bounding-box
  :struct-slots ((%low nil min)
                 (%high nil max)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass bounding-box (rl-bounding-box) ()))
