(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass vec (c-struct linkable) ()))

(define-print-object vec
    ())


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-vector2 (vec)
    ()
    (:default-initargs
     :c-ptr (calloc 'claylib/ll:vector2))))

(defcreader x rl-vector2 x vector2)
(defcreader y rl-vector2 y vector2)

(define-print-object rl-vector2
  (x y))

(defcwriter x rl-vector2 x vector2 number float)
(defcwriter y rl-vector2 y vector2 number float)

(definitializer rl-vector2
  :pt-accessors ((x number float)
                 (y number float)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass vector2 (rl-vector2) ()))

(define-print-object vector2
    ())

(defun make-vector2 (x y)
  (make-instance 'vector2 :x x :y y))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-vector3 (vec)
    ()
    (:default-initargs
     :c-ptr (calloc 'claylib/ll:vector3))))

(defcreader x rl-vector3 x vector3)
(defcreader y rl-vector3 y vector3)
(defcreader z rl-vector3 z vector3)

(define-print-object rl-vector3
    (x y z))

(defcwriter x rl-vector3 x vector3 number float)
(defcwriter y rl-vector3 y vector3 number float)
(defcwriter z rl-vector3 z vector3 number float)

(definitializer rl-vector3
  :pt-accessors ((x number float)
                 (y number float)
                 (z number float)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass vector3 (rl-vector3) ()))

(define-print-object vector3
    ())

(defun make-vector3 (x y z)
  (make-instance 'vector3 :x x :y y :z z))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-vector4 (vec)
    ()
    (:default-initargs
     :c-ptr (calloc 'claylib/ll:vector4))))

(defcreader x rl-vector4 x vector4)
(defcreader y rl-vector4 y vector4)
(defcreader z rl-vector4 z vector4)
(defcreader w rl-vector4 w vector4)

(define-print-object rl-vector4
    (x y z w))

(defcwriter x rl-vector4 x vector4 number float)
(defcwriter y rl-vector4 y vector4 number float)
(defcwriter z rl-vector4 z vector4 number float)
(defcwriter w rl-vector4 w vector4 number float)

(definitializer rl-vector4
  :pt-accessors ((x number float)
                 (y number float)
                 (z number float)
                 (w number float)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass vector4 (rl-vector4) ()))

(define-print-object vector4
    ())

(defun make-vector4 (x y z w)
  (make-instance 'vector4 :x x :y y :z z :w w))
