(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-button (gui-object text-label pressable) ()
    (:documentation "Button control, sets PRESSED when clicked")))

(define-print-object gui-button
    ())

(defun-pt-bool gui-button claylib/ll:gui-button
  "Button control, returns T when clicked"
  (bounds rl-rectangle)
  (text string))

(defmethod draw-object ((obj gui-button))
  (setf (slot-value obj '%pressed)
        (gui-button (bounds obj) (text obj))))

(defun make-gui-button (x y width height &rest args &key text)
  (declare (ignorable text))
  (apply #'make-instance 'gui-button
         :bounds (make-simple-rec x y width height)
         args))
