(in-package #:claylib)

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
   ;; TODO review: is it reasonable to reuse rot-axis as the "up" vector? It's not that clear but
   ;; inheriting most behaviour from game-obect is convenient
   (%rot-axis :documentation "The vector which defines the \"up\" direction for this billboard.")
   (%rot-angle :documentation "The counter-clockwise rotation of the billboard (as it appears to the
camera).")
   (%origin :initarg :origin
            :type rl-vector3
            :accessor origin)
   (%tint :initarg :tint
          :type rl-color
          :accessor tint)))

(defreader c-asset billboard c-asset asset)
(defreader y-scale 3d-object x size)
(defreader x-scale 3d-object y size)

(defwriter y-scale 3d-object x size number)
(defwriter x-scale 3d-object y size number)

(default-slot-value billboard %origin (make-vector3 0 0 0))
(default-slot-value billboard %tint +white+)
(default-slot-value billboard %rot-axis (make-vector3 0 1 0))

(defun make-billboard (texture-asset camera x y z x-scale y-scale source
                       &rest args &key rot-axis rot-angle origin tint)
  (declare (ignore rot-axis rot-angle origin tint))
  (load-asset texture-asset)
  (apply #'make-instance 'billboard
         :allow-other-keys t
         :asset texture-asset
         :camera camera
         :pos (make-vector3 x y z)
         :size (make-vector2 y-scale x-scale)
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
