(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-toggle (gui-object text-label)
    ((%active :initarg :active
              :type boolean
              :accessor active))
    (:default-initargs
     :active nil)
    (:documentation "Toggle Button control, sets ACTIVE when active")))

(define-print-object gui-toggle
  (active))

(defun-pt-bool gui-toggle claylib/ll:gui-toggle
  "Toggle Button control, returns T when active"
  (bounds rl-rectangle)
  (text string)
  (active boolean))

(defmethod draw-object ((obj gui-toggle))
  (setf (active obj)
        (gui-toggle (bounds obj) (text obj) (active obj))))

(defun make-gui-toggle (x y width height &rest args &key text active)
  (declare (ignorable text active))
  (apply #'make-instance 'gui-toggle
         :bounds (make-simple-rec x y width height)
         args))
