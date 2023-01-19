(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass circle (2d-shape)
    ((%radius :initarg :radius
              :type number
              :reader radius))))

(defwriter-float radius circle)

(define-print-object circle
    (radius))

(definitializer circle
  :lisp-slots ((%radius t)))

(defun make-circle (x y radius color
                    &rest args &key color2 filled)
  (declare (ignorable color2 filled))
  (apply #'make-instance 'circle
         :pos (make-vector2 x y)
         :radius radius
         :color color
         args))

(defmethod draw-object ((obj circle))
  (cond
    ((and (filled obj) (slot-boundp obj '%color2))
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

(defmethod image-draw (image (obj circle))
  (claylib/ll:image-draw-circle (c-struct image)
                                (truncate (x obj))
                                (truncate (y obj))
                                (truncate (radius obj))
                                (c-struct (color obj))))
