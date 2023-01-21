(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-label-button (gui-object text-label pressable) ()
    (:documentation "Label Button control, sets PRESSED when clicked")))

(define-print-object gui-label-button
    ())

(defun-pt-bool gui-label-button claylib/ll:gui-label-button
  "Label button control, returns T when clicked"
  (bounds rl-rectangle)
  (text string))

(defmethod draw-object ((obj gui-label-button))
  (setf (slot-value obj '%pressed)
        (gui-label-button (bounds obj) (text obj))))

(defun make-gui-label-button (x y width height text)
  (make-instance 'gui-label-button
                 :bounds (make-simple-rec x y width height)
                 :text text))
