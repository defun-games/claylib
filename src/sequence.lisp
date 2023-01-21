(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-sequence (sequences:sequence standard-object)
    ((%cl-array :type array
                :initarg :cl-array
                :reader cl-array
                :documentation "An array of RL-* objects tracking C array elements underneath."))))

(define-print-object rl-sequence
    (cl-array))

(defmethod sequences:length ((sequence rl-sequence))
  (length (cl-array sequence)))

(defmethod sequences:elt ((sequence rl-sequence) index)
  (elt (cl-array sequence) index))
