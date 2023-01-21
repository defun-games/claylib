(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-panel (gui-object text-label) ()
    (:documentation "Panel control, useful to group controls")))

(define-print-object gui-panel
  ())

(defun-pt-void gui-panel claylib/ll:gui-panel
  "Panel control, useful to group controls"
  (bounds rl-rectangle)
  (text string))

(defmethod draw-object ((obj gui-panel))
  (gui-panel (bounds obj) (text obj)))

(defun make-gui-panel (x y width height &rest args &key text)
  (declare (ignorable text))
  (apply #'make-instance 'gui-panel
         :bounds (make-simple-rec x y width height)
         args))
