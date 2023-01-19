(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-status-bar (gui-object text-label) ()
    (:documentation "Status Bar control, shows info text")))

(define-print-object gui-status-bar
    ())

(defun-pt-void gui-status-bar claylib/ll:gui-status-bar
  "Status Bar control, shows info text"
  (bounds rl-rectangle)
  (text string))

(defmethod draw-object ((obj gui-status-bar))
  (gui-status-bar (bounds obj) (text obj)))

(defun make-gui-status-bar (x y width height &rest args &key text)
  (declare (ignorable text))
  (apply #'make-instance 'gui-status-bar
         :bounds (make-simple-rec x y width height)
         args))
