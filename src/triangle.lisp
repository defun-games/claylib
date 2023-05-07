(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass tri (linkable)
    ((%v1 :initarg :v1
          :type vec
          :accessor v1)
     (%v2 :initarg :v2
          :type vec
          :accessor v2)
     (%v3 :initarg :v3
          :type vec
          :accessor v3))))

(defreader x1 tri x v1)
(defreader y1 tri y v1)
(defreader x2 tri x v2)
(defreader y2 tri y v2)
(defreader x3 tri x v3)
(defreader y3 tri y v3)

(defwriter x1 tri x v1 number)
(defwriter y1 tri y v1 number)
(defwriter x2 tri x v2 number)
(defwriter y2 tri y v2 number)
(defwriter x3 tri x v3 number)
(defwriter y3 tri y v3 number)

(child-setter tri v1 v2 v3)

(define-print-object tri
  (v1 v2 v3))

(definitializer tri
  :lisp-slots ((%v1) (%v2) (%v3)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass triangle (2d-shape tri)
    ((%v1 :type rl-vector2)
     (%v2 :type rl-vector2)
     (%v3 :type rl-vector2))))

(definitializer triangle
  :lisp-slots ((%v1) (%v2) (%v3)))

(defun make-triangle (x1 y1 x2 y2 x3 y3 color
                      &rest args &key filled)
  (declare (ignorable filled))
  (apply #'make-instance 'triangle
         :v1 (make-vector2 x1 y1)
         :v2 (make-vector2 x2 y2)
         :v3 (make-vector2 x3 y3)
         :color color
         args))

(defun make-triangle-from-vecs (v1 v2 v3 color
                                &rest args &key filled)
  (declare (ignorable filled))
  (apply #'make-instance 'triangle
         :v1 v1
         :v2 v2
         :v3 v3
         :color color
         args))

(defmethod draw-object ((obj triangle))
  (let ((v1 (c-ptr (v1 obj)))
        (v2 (c-ptr (v2 obj)))
        (v3 (c-ptr (v3 obj)))
        (color (c-ptr (color obj))))
    (if (filled obj)
        (claylib/ll:draw-triangle v1 v2 v3 color)
        (claylib/ll:draw-triangle-lines v1 v2 v3 color))))

(static-draw draw-triangle-object triangle)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass triangle-3d (3d-shape tri)
    ((%v1 :type rl-vector3)
     (%v2 :type rl-vector3)
     (%v3 :type rl-vector3))))

(defreader z1 triangle-3d z v1)
(defreader z2 triangle-3d z v2)
(defreader z3 triangle-3d z v3)

(defwriter z1 triangle-3d z v1 number)
(defwriter z2 triangle-3d z v2 number)
(defwriter z3 triangle-3d z v3 number)

(definitializer triangle-3d
  :lisp-slots ((%v1) (%v2) (%v3)))

(defun make-triangle-3d (x1 y1 z1 x2 y2 z2 x3 y3 z3 color
                         &rest args)
  (apply #'make-instance 'triangle-3d
         :v1 (make-vector3 x1 y1 z1)
         :v2 (make-vector3 x2 y2 z2)
         :v3 (make-vector3 x3 y3 z3)
         :color color
         args))

(defun make-triangle-3d-from-vecs (v1 v2 v3 color
                                   &rest args)
  (apply #'make-instance 'triangle-3d
         :v1 v1
         :v2 v2
         :v3 v3
         :color color
         args))

(defmethod draw-object ((obj triangle-3d))
  (claylib/ll:draw-triangle-3d (c-ptr (v1 obj))
                               (c-ptr (v2 obj))
                               (c-ptr (v3 obj))
                               (c-ptr (color obj))))

(static-draw draw-triangle-3d-object triangle-3d)
