(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass triangle (2d-shape)
    ((%v1 :initarg :v1
          :type rl-vector2
          :accessor v1)
     (%v2 :initarg :v2
          :type rl-vector2
          :accessor v2)
     (%v3 :initarg :v3
          :type rl-vector2
          :accessor v3))))

(defreader x1 triangle x v1)
(defreader y1 triangle y v1)
(defreader x2 triangle x v2)
(defreader y2 triangle y v2)
(defreader x3 triangle x v3)
(defreader y3 triangle y v3)

(define-print-object triangle
    (v1 v2 v3))

(defwriter x1 triangle x v1 number)
(defwriter y1 triangle y v1 number)
(defwriter x2 triangle x v2 number)
(defwriter y2 triangle y v2 number)
(defwriter x3 triangle x v3 number)
(defwriter y3 triangle y v3 number)

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
  (let ((v1 (c-struct (v1 obj)))
        (v2 (c-struct (v2 obj)))
        (v3 (c-struct (v3 obj)))
        (color (c-struct (color obj))))
    (if (filled obj)
        (claylib/ll:draw-triangle v1 v2 v3 color)
        (claylib/ll:draw-triangle-lines v1 v2 v3 color))))
