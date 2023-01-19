(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass billboard (3d-object)
    ((%asset :initarg :asset
             :type texture-asset
             :accessor asset)
     (%camera :initarg :camera
              :type rl-camera-3d
              :accessor camera
              :documentation "The camera to view the billboard from.")
     (%size :initarg :size
            :type rl-vector2
            :accessor size)
     (%source :initarg :source
              :type rl-rectangle
              :accessor source)
     (%rot-axis :initarg :up
                :accessor up
                :documentation "The vector which defines the \"up\" direction for this billboard.")
     (%rot-angle :documentation "The counter-clockwise rotation of the billboard (as it appears to the
camera).")
     (%origin :initarg :origin
              :type rl-vector3
              :accessor origin)
     (%tint :initarg :tint
            :type rl-color
            :accessor tint))
    (:default-initargs
     :size (make-vector2 1 1)
     :rot-axis (make-vector3 0 1 0)
     :origin (make-vector3 0 0 0)
     :tint +white+)))

(defreader c-asset billboard c-asset asset)
(defreader x-scale billboard x size)
(defreader y-scale billboard y size)

(define-print-object billboard
    (asset camera size source up origin tint c-asset x-scale y-scale))

(defwriter x-scale billboard x size number)
(defwriter y-scale billboard y size number)

(definitializer billboard
  :lisp-slots ((%asset) (%camera) (%size) (%source) (%rot-axis) (%rot-angle) (%origin) (%tint)))

(defun make-billboard (texture-asset camera x y z x-scale y-scale source
                       &rest args &key up rot-angle origin tint)
  (declare (ignore up rot-angle origin tint))
  (load-asset texture-asset)
  (apply #'make-instance 'billboard
         :allow-other-keys t
         :asset texture-asset
         :camera camera
         :pos (make-vector3 x y z)
         :size (make-vector2 x-scale y-scale)
         :source source
         args))

(defmethod draw-object ((obj billboard))
  (claylib/ll:draw-billboard-pro (c-struct (camera obj))
                                 (c-asset obj)
                                 (c-struct (source obj))
                                 (c-struct (pos obj))
                                 (c-struct (rot-axis obj))
                                 (c-struct (size obj))
                                 (c-struct (origin obj))
                                 (rot-angle obj)
                                 (c-struct (tint obj))))
