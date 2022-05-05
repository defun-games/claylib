(in-package #:claylib)

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
    :accessor c-struct)))

(defreader x rl-ray x pos)
(defreader y rl-ray y pos)
(defreader z rl-ray z pos)

(defwriter x rl-ray x pos number)
(defwriter y rl-ray y pos number)
(defwriter z rl-ray z pos number)
(defcwriter-struct pos rl-ray position ray vector3 x y z)
(defcwriter-struct dir rl-ray direction ray vector3 x y z)

(definitializer rl-ray (pos rl-vector3) (dir rl-vector3))

(default-free rl-ray)
(default-free-c claylib/ll:ray)



(defclass ray (rl-ray)
  ((%color :initarg :color
           :type rl-color
           :accessor color)))

(definitializer ray (color rl-color nil))

(default-free ray)

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

(defun get-mouse-ray (mouse-pos camera &optional ray)
  "Gets a mouse ray for the passed mouse position and camera. Allocates a new RAY unless you pass one."
  (check-type mouse-pos rl-vector2)
  (check-type camera camera-3d)
  (check-type ray (or null ray))
  (let ((ray (or ray (make-ray 0 0 0 0 0 0 +black+))))
    (claylib/ll:get-mouse-ray (c-struct ray)
                              (c-struct mouse-pos)
                              (c-struct camera))
    ray))
