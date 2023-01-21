(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-window-box (gui-object pressable)
    ((%title :initarg :title
             :type string
             :accessor title))
    (:default-initargs
     :title "")
    (:documentation "Window Box control, shows a window that can be closed")))

(define-print-object gui-window-box
    (title))

(defun-pt-bool gui-window-box claylib/ll:gui-window-box
  "Window Box control, shows a window that can be closed"
  (bounds rl-rectangle)
  (title string))

(defmethod draw-object ((obj gui-window-box))
  (setf (slot-value obj '%pressed)
        (gui-window-box (bounds obj) (title obj))))

(defun make-gui-window-box (x y width height &rest args &key title)
  (declare (ignorable title))
  (apply #'make-instance 'gui-window-box
         :bounds (make-simple-rec x y width height)
         args))
