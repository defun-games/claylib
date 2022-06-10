(in-package #:claylib)

(defclass pixel (2d-shape) ())

(defun make-pixel (x y color)
  (make-instance 'pixel
                 :pos (make-vector2 x y)
                 :color color))

(defmethod draw-object ((obj pixel))
  (claylib/ll:draw-pixel (truncate (x obj))
                         (truncate (y obj))
                         (c-struct (color obj))))
