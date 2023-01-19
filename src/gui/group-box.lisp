(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-group-box (gui-object text-label) ()
    (:documentation "Group Box control with text name")))

(define-print-object gui-group-box
    ())

(defun-pt-void gui-group-box claylib/ll:gui-group-box
  "Group Box control with text name"
  (bounds rl-rectangle)
  (text string))

(defmethod draw-object ((obj gui-group-box))
  (gui-group-box (bounds obj) (text obj)))

(defun make-gui-group-box (x y width height &rest args &key text)
  (declare (ignorable text))
  (apply #'make-instance 'gui-group-box
         :bounds (make-simple-rec x y width height)
         args))
