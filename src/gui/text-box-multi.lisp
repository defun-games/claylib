(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-text-box-multi (gui-text-box) ()
    (:documentation "Text Box control with multiple lines")))

(define-print-object gui-text-box-multi
    ())

(defun-pt-bool gui-text-box-multi claylib/ll:gui-text-box-multi
  "Text Box control with multiple lines"
  (bounds rl-rectangle)
  (text cffi:foreign-pointer)
  (text-size integer)
  (edit-mode boolean))

(defmethod draw-object ((obj gui-text-box-multi))
  (setf (slot-value obj '%pressed)
        (gui-text-box-multi (bounds obj)
                            (slot-value obj '%text)
                            (text-size obj)
                            (edit-mode obj))))

(defun make-gui-text-box-multi (x y width height text-size &rest args &key text edit-mode)
  (declare (ignorable text edit-mode))
  (apply #'make-instance 'gui-text-box-multi
         :bounds (make-simple-rec x y width height)
         :text-size text-size
         args))
