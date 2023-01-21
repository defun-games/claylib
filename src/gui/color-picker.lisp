(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-color-picker (gui-object text-label gui-color) ()
    (:documentation "Color Picker control (multiple color controls), sets COLOR to chosen color")))

(define-print-object gui-color-picker
    ())

(defun-pt gui-color-picker claylib/ll:gui-color-picker
  "Color Picker control (multiple color controls). Allocates a new RL-COLOR unless you pass one."
  (col rl-color nil (make-color 0 0 0))
  (bounds rl-rectangle)
  (text string)
  (color rl-color))

(defmethod draw-object ((obj gui-color-picker))
  (gui-color-picker (bounds obj)
                    (text obj)
                    (color obj)
                    :col (color obj)))

(defun make-gui-color-picker (x y width height color &rest args &key text)
  (declare (ignorable text))
  (apply #'make-instance 'gui-color-picker
         :bounds (make-simple-rec x y width height)
         :color color
         args))
