(in-package #:claylib)

(defclass rl-camera-3d ()
  ((%position :initarg :pos
              :type rl-vector3
              :reader pos)
   (%target :initarg :target
            :type rl-vector3
            :reader target)
   (%up :initarg :up
        :type rl-vector3
        :reader up)
   (%c-struct
    :type claylib/ll:camera3d
    :initform (autowrap:alloc 'claylib/ll:camera3d)
    :accessor c-struct)))

(defreader x rl-camera-3d x pos)
(defreader y rl-camera-3d y pos)
(defreader z rl-camera-3d z pos)
(defcreader fovy rl-camera-3d fovy camera3d)
(defcreader projection rl-camera-3d projection camera3d)

(defwriter x rl-camera-3d x pos number)
(defwriter y rl-camera-3d y pos number)
(defwriter z rl-camera-3d z pos number)
(defcwriter fovy rl-camera-3d fovy camera3d number float)
(defcwriter projection rl-camera-3d projection camera3d integer)
(defcwriter-struct pos rl-camera-3d position camera3d vector3 x y z)
(defcwriter-struct target rl-camera-3d target camera3d vector3 x y z)
(defcwriter-struct up rl-camera-3d up camera3d vector3 x y z)

(definitializer rl-camera-3d
    (pos rl-vector3) (target rl-vector3) (up rl-vector3) (fovy number float) (projection integer))

(default-free rl-camera-3d)
(default-free-c claylib/ll:camera3d)



(defclass camera-3d (rl-camera-3d)
  ((%mode :initarg :mode
          :type integer
          :reader mode)))

(defmethod (setf mode) ((value integer) (camera camera-3d))
  (claylib/ll:set-camera-mode (c-struct camera) value)
  (setf (slot-value camera '%mode) value))

(definitializer camera-3d (mode integer))

(defun make-camera-3d (pos-x pos-y pos-z
                       target-x target-y target-z
                       up-x up-y up-z
                       &key
                         (fovy 45.0)
                         (projection +camera-perspective+)
                         (mode +camera-custom+))
  (make-instance 'camera-3d
                 :pos (make-vector3 pos-x pos-y pos-z)
                 :target (make-vector3 target-x target-y target-z)
                 :up (make-vector3 up-x up-y up-z)
                 :fovy fovy
                 :projection projection
                 :mode mode))

(defun make-camera-3d-from-vecs (pos target up
                                 &key
                                   (fovy 45.0)
                                   (projection +camera-perspective+)
                                   (mode +camera-custom+))
  (make-instance 'camera-3d
                 :pos pos
                 :target target
                 :up up
                 :fovy fovy
                 :projection projection
                 :mode mode))

(defun update-camera (camera)
  (claylib/ll:update-camera (c-struct camera)))

(defun-pt get-world-to-screen-3d claylib/ll:get-world-to-screen-ex
  "Get world-to-screen transform of a 3D camera. Allocates a new VECTOR2 unless you pass one."
  (vec rl-vector2 nil (make-vector2 0 0))
  (position rl-vector3)
  (camera camera-3d)
  (width integer nil *screen-width*)
  (height integer nil *screen-height*))
