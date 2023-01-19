(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-color-panel (gui-object text-label gui-color) ()
    (:documentation "Color Panel control, sets COLOR to chosen color")))

(define-print-object gui-color-panel
    ())

(defun-pt gui-color-panel claylib/ll:gui-color-panel
  "Color Panel control. Allocates a new RL-COLOR unless you pass one."
  (col rl-color nil (make-color 0 0 0))
  (bounds rl-rectangle)
  (text string)
  (color rl-color))

(defmethod draw-object ((obj gui-color-panel))
  (gui-color-panel (bounds obj)
                   (text obj)
                   (color obj)
                   :col (color obj)))

(defun make-gui-color-panel (x y width height color &rest args &key text)
  (declare (ignorable text))
  (apply #'make-instance 'gui-color-panel
         :bounds (make-simple-rec x y width height)
         :color color
         args))
