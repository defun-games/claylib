(in-package #:claylib)

(defclass circle (2d-shape)
  ((%radius :initarg :radius
            :type (or integer float)
            :reader radius)))

(defwriter-float radius circle)

(definitializer-float circle radius)

(defun make-circle (x y radius color &key (color2 nil) (filled t))
  (make-instance 'circle
                 :pos (make-vector2 x y)
                 :radius radius
                 :color color
                 :color2 color2
                 :filled filled))

(defmethod draw-object ((obj circle))
  (cond
    ((and (filled obj) (color2 obj))
     (claylib/ll:draw-circle-gradient (truncate (x obj))
                                      (truncate (y obj))
                                      (radius obj)
                                      (c-struct (color obj))
                                      (c-struct (color2 obj))))
    ((filled obj)
     (claylib/ll:draw-circle-v (c-struct (pos obj))
                                (radius obj)
                                (c-struct (color obj))))
    (t
     ;; There's no DRAW-CIRCLE-LINES-V, so we have to use ints :-(
      (claylib/ll:draw-circle-lines (truncate (x obj))
                                    (truncate (y obj))
                                    (radius obj)
                                    (c-struct (color obj))))))
