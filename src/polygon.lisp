(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass polygon (2d-shape)
    ((%sides :initarg :sides
             :type integer
             :accessor sides)
     (%radius :initarg :radius
              :type number
              :reader radius)
     (%rotation :initarg :rot
                :type number
                :reader rot)
     (%thickness :initarg :thickness
                 :type number
                 :reader thickness))
    (:default-initargs
     :thickness 1.0
     :rot 0.0)))

(define-print-object polygon
    (sides radius rot thickness))

(defwriter-float radius polygon)
(defwriter-float rot polygon %rotation)
(defwriter-float thickness polygon)

(child-setter polygon sides)

(definitializer polygon
  :lisp-slots ((%sides)
               (%radius t)
               (%rotation t)
               (%thickness t)))

(defun make-polygon (x y sides radius color
                     &rest args &key rotation thickness filled)
  (apply #'make-instance 'polygon
         :pos (make-vector2 x y)
         :sides sides
         :radius radius
         :color color
         args))

(defmethod draw-object ((obj polygon))
  (with-accessors ((center pos) (sides sides) (radius radius) (rot rot) (color color)
                   (thickness thickness)) obj
      (if (filled obj)
          (claylib/ll:draw-poly (c-ptr center) sides radius rot (c-ptr color))
          (claylib/ll:draw-poly-lines-ex
           (c-ptr center) sides radius rot thickness (c-ptr color)))))

(static-draw draw-polygon-object polygon)
