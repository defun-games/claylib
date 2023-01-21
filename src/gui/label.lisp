(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-label (gui-object text-label) ()
    (:documentation "Label control, shows text")))

(define-print-object gui-label
    ())

(defun-pt-void gui-label claylib/ll:gui-label
  "Label control, shows text"
  (bounds rl-rectangle)
  (text string))

(defmethod draw-object ((obj gui-label))
  (gui-label (bounds obj) (text obj)))

(defun make-gui-label (x y width height text)
  (make-instance 'gui-label
                 :bounds (make-simple-rec x y width height)
                 :text text))
