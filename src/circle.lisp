(in-package #:claylib)

(defclass circle (2d-shape)
  ((%radius :initarg :radius
            :type (or integer float)
            :reader radius)))

(defwriter-float radius circle)

(definitializer-float circle radius)

(defun make-circle (x y radius color &key (filled t))
  (make-instance 'circle
                 :x x :y y
                 :pos (make-vector2 x y)
                 :radius radius
                 :color color
                 :filled filled))

(defmethod draw-object ((obj circle))
  (if (filled obj)
      (claylib/ll:draw-circle-v (c-struct (pos obj))
                                (radius obj)
                                (c-struct (color obj)))
      ;; There's no DRAW-CIRCLE-LINES-V, so we have to use ints :-(
      (claylib/ll:draw-circle-lines (truncate (x obj))
                                    (truncate (y obj))
                                    (radius obj)
                                    (c-struct (color obj)))))
