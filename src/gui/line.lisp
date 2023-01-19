(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-line (gui-object text-label) ()
    (:documentation "Line separator control, could contain text")))

(define-print-object gui-line
    ())

(defun-pt-void gui-line claylib/ll:gui-line
  "Line separator control, could contain text"
  (bounds rl-rectangle)
  (text string))

(defmethod draw-object ((obj gui-line))
  (gui-line (bounds obj) (text obj)))

(defun make-gui-line (x y width height &rest args &key text)
  (declare (ignorable text))
  (apply #'make-instance 'gui-line
         :bounds (make-simple-rec x y width height)
         args))
