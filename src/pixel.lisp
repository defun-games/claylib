(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass pixel (2d-shape) ()))

(define-print-object pixel
    ())

(defun make-pixel (x y color)
  (make-instance 'pixel
                 :pos (make-vector2 x y)
                 :color color))

(defmethod draw-object ((obj pixel))
  (claylib/ll:draw-pixel (truncate (x obj))
                         (truncate (y obj))
                         (c-ptr (color obj))))

(static-draw draw-pixel-object pixel)

(defmethod image-draw (image (obj pixel))
  (claylib/ll:image-draw-pixel (c-ptr image)
                               (truncate (x obj))
                               (truncate (y obj))
                               (c-ptr (color obj))))
