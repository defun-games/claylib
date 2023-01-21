(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-text-box (gui-object text-box pressable editable) ()
    (:documentation "Text Box control, updates input text, sets PRESSED when entered")))

(define-print-object gui-text-box
    ())

(defun-pt-bool gui-text-box claylib/ll:gui-text-box
  "Text Box control, updates input text"
  (bounds rl-rectangle)
  (text cffi:foreign-pointer)
  (text-size integer)
  (edit-mode boolean))

(defmethod draw-object ((obj gui-text-box))
  (setf (slot-value obj '%pressed)
        (gui-text-box (bounds obj)
                      (slot-value obj '%text)
                      (text-size obj)
                      (edit-mode obj))))

(defun make-gui-text-box (x y width height text-size &rest args &key text edit-mode)
  (declare (ignorable text edit-mode))
  (apply #'make-instance 'gui-text-box
         :bounds (make-simple-rec x y width height)
         :text-size text-size
         args))
