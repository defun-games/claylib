(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-checkbox (gui-object text-label)
    ((%checked :initarg :checked
               :type boolean
               :accessor checked))
    (:default-initargs
     :checked nil)
    (:documentation "Checkbox control, sets CHECKED when active")))

(define-print-object gui-checkbox
    (checked))

(defun-pt-bool gui-checkbox claylib/ll:gui-checkbox
  "Checkbox control, returns T when active"
  (bounds rl-rectangle)
  (text string)
  (checked boolean))

(defmethod draw-object ((obj gui-checkbox))
  (setf (checked obj)
        (gui-checkbox (bounds obj) (text obj) (checked obj))))

(defun make-gui-checkbox (x y width height &rest args &key text checked)
  (declare (ignorable text checked))
  (apply #'make-instance 'gui-checkbox
         :bounds (make-simple-rec x y width height)
         args))
