(in-package #:claylib)

(defclass game-object ()
  ((%position :initarg :pos
              :accessor pos)))



(defclass 2d-object (game-object)
  ((%position :type rl-vector2)))

(defreader x 2d-object x pos)
(defreader y 2d-object y pos)

(defwriter x 2d-object x pos number)
(defwriter y 2d-object y pos number)

(definitializer 2d-object (pos rl-vector2 nil))

(defmethod free ((obj 2d-object))
  (free (pos obj))
  (setf (pos obj) nil)
  (when (next-method-p)
    (call-next-method)))



(defclass 3d-object (game-object)
  ((%position :type rl-vector3)
   (%rot-axis :initarg :rot-axis
              :type rl-vector3
              :accessor rot-axis)
   (%rot-angle :initarg :rot-angle
               :type (or integer float)
               :reader rot-angle)))

(defreader x 3d-object x pos)
(defreader y 3d-object y pos)
(defreader z 3d-object z pos)

(defwriter x 3d-object x pos number)
(defwriter y 3d-object y pos number)
(defwriter z 3d-object z pos number)
(defwriter-float rot-angle 3d-object)

(definitializer 3d-object
    (pos rl-vector3 nil) (rot-axis rl-vector3 nil) (rot-angle number float))

(default-slot-value 3d-object %rot-axis (make-vector3 0 0 0))
(default-slot-value 3d-object %rot-angle 0.0)

(defmethod free ((obj 3d-object))
  (mapcar #'free (list (pos obj) (rot-axis obj)))
  (setf (pos obj) nil
        (rot-axis obj) nil)
  (when (next-method-p)
    (call-next-method)))
