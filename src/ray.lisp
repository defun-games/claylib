(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-ray ()
    ((%position :initarg :pos
                :type rl-vector3
                :reader pos)
     (%direction :initarg :dir
                 :type rl-vector3
                 :reader dir)
     (%c-struct
      :type claylib/ll:ray
      :initform (autowrap:alloc 'claylib/ll:ray)
      :accessor c-struct))))

(defreader x rl-ray x pos)
(defreader y rl-ray y pos)
(defreader z rl-ray z pos)

(defwriter x rl-ray x pos number)
(defwriter y rl-ray y pos number)
(defwriter z rl-ray z pos number)
(defcwriter-struct pos rl-ray position ray vector3 x y z)
(defcwriter-struct dir rl-ray direction ray vector3 x y z)

(defmethod sync-children ((obj rl-ray))
  (unless (eq (c-struct (pos obj))
              (ray.position (c-struct obj)))
    (free-later (c-struct (pos obj)))
    (setf (c-struct (pos obj))
          (ray.position (c-struct obj))))
  (unless (eq (c-struct (dir obj))
              (ray.direction (c-struct obj)))
    (free-later (c-struct (dir obj)))
    (setf (c-struct (dir obj))
          (ray.direction (c-struct obj)))))

(definitializer rl-ray
  :struct-slots ((%position) (%direction)))

(default-free rl-ray %position %direction)
(default-free-c claylib/ll:ray)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass ray (rl-ray)
    ((%color :initarg :color
             :type rl-color
             :accessor color))))

(definitializer ray
  :lisp-slots ((%color)))

(default-free ray %color)

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
  (claylib/ll:draw-ray (c-struct obj) (c-struct (color obj))))
