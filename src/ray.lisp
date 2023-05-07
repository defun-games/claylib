(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-ray (c-struct linkable)
    ((%position :initarg :pos
                :type rl-vector3
                :reader pos)
     (%direction :initarg :dir
                 :type rl-vector3
                 :reader dir))
    (:default-initargs
     :c-ptr (calloc 'claylib/ll:ray))))

(defreader x rl-ray x pos)
(defreader y rl-ray y pos)
(defreader z rl-ray z pos)

(define-print-object rl-ray
    (pos dir))

(defwriter x rl-ray x pos number)
(defwriter y rl-ray y pos number)
(defwriter z rl-ray z pos number)
(defcwriter-struct pos rl-ray position ray vector3 x y z)
(defcwriter-struct dir rl-ray direction ray vector3 x y z)

(definitializer rl-ray
  :struct-slots ((%position) (%direction)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass ray (rl-ray)
    ((%color :initarg :color
             :type rl-color
             :accessor color))))

(define-print-object ray
    (color))

(definitializer ray
  :lisp-slots ((%color)))

(defun make-ray (pos-x pos-y pos-z dir-x dir-y dir-z color)
  (make-instance 'ray
                 :pos (make-vector3 pos-x pos-y pos-z)
                 :dir (make-vector3 dir-x dir-y dir-z)
                 :color color))

(defun make-ray-from-vecs (pos dir color)
  (make-instance 'ray
                 :pos pos
                 :dir dir
                 :color color))

(defmethod draw-object ((obj ray))
  (claylib/ll:draw-ray (c-ptr obj) (c-ptr (color obj))))

(static-draw draw-ray-object ray)
