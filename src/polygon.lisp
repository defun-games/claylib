(in-package #:claylib)

(defclass polygon (2d-shape)
  ((%sides :initarg :sides
           :type integer
           :accessor sides)
   (%radius :initarg :radius
            :type (or integer float)
            :reader radius)
   (%rotation :initarg :rot
              :type (or integer float)
              :reader rot)
   (%thickness :initarg :thickness
               :type (or integer float)
               :reader thickness)))

(defwriter-float radius polygon)
(defwriter-float rot polygon %rotation)
(defwriter-float thickness polygon)

(definitializer polygon
    (sides integer nil) (radius number float) (rot number float) (thickness number float 1.0))

(defun make-polygon (x y sides radius color
                     &key (filled t) (rotation 0) (thickness 1))
  (make-instance 'polygon
                 :pos (make-vector2 x y)
                 :sides sides
                 :radius radius
                 :color color
                 :filled filled
                 :rot rotation
                 :thickness thickness))

(defmethod draw-object ((obj polygon))
  (with-accessors ((center pos) (sides sides) (radius radius) (rot rot) (color color)
                   (thickness thickness)) obj
      (if (filled obj)
          (claylib/ll:draw-poly (c-struct center) sides radius rot (c-struct color))
          (claylib/ll:draw-poly-lines-ex
           (c-struct center) sides radius rot thickness (c-struct color)))))
