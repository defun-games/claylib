(in-package #:claylib)

(defclass rl-bone-info ()
  ((%c-struct
    :type claylib/ll:bone-info
    :initform (autowrap:alloc 'claylib/ll:bone-info)
    :accessor c-struct)))

(defcreader name rl-bone-info name bone-info)
(defcreader parent rl-bone-info parent bone-info)

(defcwriter name rl-bone-info name bone-info string)
(defcwriter parent rl-bone-info parent bone-info integer)

(definitializer rl-bone-info (name string) (parent integer))

(default-free rl-bone-info)
(default-free-c claylib/ll:bone-info)
