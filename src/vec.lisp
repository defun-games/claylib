(in-package #:claylib)

(defclass vec () ())

(definitializer vec)

(default-free vec)



(defclass rl-vector2 (vec)
  ((%c-struct
    :type claylib/ll:vector2
    :initform (autowrap:alloc 'claylib/ll:vector2)
    :accessor c-struct)))

(defcreader x rl-vector2 x vector2)
(defcreader y rl-vector2 y vector2)

(defcwriter x rl-vector2 x vector2 number float)
(defcwriter y rl-vector2 y vector2 number float)

(definitializer rl-vector2 (x number float) (y number float))

(default-free-c claylib/ll:vector2)

(defun make-vector2 (x y)
  (make-instance 'rl-vector2 :x x :y y))



(defclass rl-vector3 (vec)
  ((%c-struct
    :type claylib/ll:vector3
    :initform (autowrap:alloc 'claylib/ll:vector3)
    :accessor c-struct)))

(defcreader x rl-vector3 x vector3)
(defcreader y rl-vector3 y vector3)
(defcreader z rl-vector3 z vector3)

(defcwriter x rl-vector3 x vector3 number float)
(defcwriter y rl-vector3 y vector3 number float)
(defcwriter z rl-vector3 z vector3 number float)

(definitializer rl-vector3 (x number float) (y number float) (z number float))

(default-free-c claylib/ll:vector3)

(defun make-vector3 (x y z)
  (make-instance 'rl-vector3 :x x :y y :z z))



(defclass rl-vector4 (vec)
  ((%c-struct
    :type claylib/ll:vector4
    :initform (autowrap:alloc 'claylib/ll:vector4)
    :accessor c-struct)))

(defcreader x rl-vector4 x vector4)
(defcreader y rl-vector4 y vector4)
(defcreader z rl-vector4 z vector4)
(defcreader w rl-vector4 w vector4)

(defcwriter x rl-vector4 x vector4 number float)
(defcwriter y rl-vector4 y vector4 number float)
(defcwriter z rl-vector4 z vector4 number float)
(defcwriter w rl-vector4 w vector4 number float)

(definitializer rl-vector4
    (x number float) (y number float) (z number float) (w number float))

(default-free-c claylib/ll:vector4)

(defun make-vector4 (x y z w)
  (make-instance 'rl-vector4 :x x :y y :z z :w w))
