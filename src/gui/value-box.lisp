(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-value-box (gui-spinner) ()
    (:documentation "Value Box control, sets PRESSED when clicked")))

(define-print-object gui-value-box
    ())

(defun-pt-bool gui-value-box claylib/ll:gui-value-box
  "Value Box control, updates input text with numbers"
  (bounds rl-rectangle)
  (text string)
  (value integer)
  (min-value integer)
  (max-value integer)
  (edit-mode boolean))

(defmethod draw-object ((obj gui-value-box))
  (setf (slot-value obj '%pressed)
        (gui-value-box (bounds obj)
                       (text obj)
                       (value obj)
                       (min-value obj)
                       (max-value obj)
                       (edit-mode obj))))

(defun make-gui-value-box (x y width height min-value max-value &rest args &key text value edit-mode)
  (declare (ignorable text value edit-mode))
  (apply #'make-instance 'gui-value-box
         :bounds (make-simple-rec x y width height)
         :min-value min-value
         :max-value max-value
         args))
