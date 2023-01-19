(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-dummy-rec (gui-object text-label) ()
    (:documentation "Dummy control for placeholders")))

(define-print-object gui-dummy-rec
    ())

(defun-pt-void gui-dummy-rec claylib/ll:gui-dummy-rec
  "Dummy control for placeholders"
  (bounds rl-rectangle)
  (text string))

(defmethod draw-object ((obj gui-dummy-rec))
  (gui-dummy-rec (bounds obj) (text obj)))

(defun make-gui-dummy-rec (x y width height &rest args &key text)
  (declare (ignorable text))
  (apply #'make-instance 'gui-dummy-rec
         :bounds (make-simple-rec x y width height)
         args))
