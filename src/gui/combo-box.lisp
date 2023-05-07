(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-combo-box (gui-object text-label)
    ((%active :initarg :active
              :type integer
              :accessor active))
    (:default-initargs
     :active 0)
    (:documentation "Combo Box control, sets ACTIVE to selected item index")))

(define-print-object gui-combo-box
    (active))

(child-setter gui-combo-box active)

(defun-pt-num gui-combo-box claylib/ll:gui-combo-box
  "Combo Box control, returns selected item index"
  (bounds rl-rectangle)
  (text string)
  (active integer))

(defmethod draw-object ((obj gui-combo-box))
  (setf (active obj)
        (gui-combo-box (bounds obj) (text obj) (active obj))))

(static-draw draw-gui-combo-box-object gui-combo-box)

(defun make-gui-combo-box (x y width height &rest args &key text active)
  (declare (ignorable text active))
  (apply #'make-instance 'gui-combo-box
         :bounds (make-simple-rec x y width height)
         args))
