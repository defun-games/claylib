(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-matrix (c-struct linkable)
    ()
    (:default-initargs
     :c-ptr (calloc 'claylib/ll:matrix))))

(defcreader m0 rl-matrix m0 matrix)
(defcreader m1 rl-matrix m1 matrix)
(defcreader m2 rl-matrix m2 matrix)
(defcreader m3 rl-matrix m3 matrix)
(defcreader m4 rl-matrix m4 matrix)
(defcreader m5 rl-matrix m5 matrix)
(defcreader m6 rl-matrix m6 matrix)
(defcreader m7 rl-matrix m7 matrix)
(defcreader m8 rl-matrix m8 matrix)
(defcreader m9 rl-matrix m9 matrix)
(defcreader m10 rl-matrix m10 matrix)
(defcreader m11 rl-matrix m11 matrix)
(defcreader m12 rl-matrix m12 matrix)
(defcreader m13 rl-matrix m13 matrix)
(defcreader m14 rl-matrix m14 matrix)
(defcreader m15 rl-matrix m15 matrix)

(define-print-object rl-matrix
    ())

(defcwriter m0 rl-matrix m0 matrix number float)
(defcwriter m1 rl-matrix m1 matrix number float)
(defcwriter m2 rl-matrix m2 matrix number float)
(defcwriter m3 rl-matrix m3 matrix number float)
(defcwriter m4 rl-matrix m4 matrix number float)
(defcwriter m5 rl-matrix m5 matrix number float)
(defcwriter m6 rl-matrix m6 matrix number float)
(defcwriter m7 rl-matrix m7 matrix number float)
(defcwriter m8 rl-matrix m8 matrix number float)
(defcwriter m9 rl-matrix m9 matrix number float)
(defcwriter m10 rl-matrix m10 matrix number float)
(defcwriter m11 rl-matrix m11 matrix number float)
(defcwriter m12 rl-matrix m12 matrix number float)
(defcwriter m13 rl-matrix m13 matrix number float)
(defcwriter m14 rl-matrix m14 matrix number float)
(defcwriter m15 rl-matrix m15 matrix number float)

(definitializer rl-matrix
  :pt-accessors ((m0 number float)
                 (m1 number float)
                 (m2 number float)
                 (m3 number float)
                 (m4 number float)
                 (m5 number float)
                 (m6 number float)
                 (m7 number float)
                 (m8 number float)
                 (m9 number float)
                 (m10 number float)
                 (m11 number float)
                 (m12 number float)
                 (m13 number float)
                 (m14 number float)
                 (m15 number float)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass matrix (rl-matrix) ()))

(define-print-object matrix
    ())

(defun make-zero-matrix ()
  (make-instance 'matrix
                 :m0 0 :m1 0 :m2 0 :m3 0
                 :m4 0 :m5 0 :m6 0 :m7 0
                 :m8 0 :m9 0 :m10 0 :m11 0
                 :m12 0 :m13 0 :m14 0 :m15 0))
