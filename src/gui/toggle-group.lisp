(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-toggle-group (gui-object text-label)
    ((%active :initarg :active
              :type integer
              :accessor active))
    (:default-initargs
     :active 0)
    (:documentation "Toggle Group control, sets ACTIVE to active toggle index")))

(child-setter gui-toggle-group active)

(define-print-object gui-toggle-group
    (active))

(defun-pt-num gui-toggle-group claylib/ll:gui-toggle-group
  "Toggle Group control, returns active toggle index"
  (bounds rl-rectangle)
  (text string)
  (active integer))

(defmethod draw-object ((obj gui-toggle-group))
  (setf (active obj)
        (gui-toggle-group (bounds obj) (text obj) (active obj))))

(static-draw draw-gui-toggle-group-object gui-toggle-group)

(defun make-gui-toggle-group (x y width height &rest args &key text active)
  (declare (ignorable text active))
  (apply #'make-instance 'gui-toggle-group
         :bounds (make-simple-rec x y width height)
         args))
