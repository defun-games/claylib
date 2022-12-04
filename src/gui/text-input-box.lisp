(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-text-input-box (gui-message-box text-label)
    ((%text-max-size :initarg :text-max-size
                     :type integer
                     :accessor text-max-size)
     (%secret-view-active :initarg :secret-view-active
                          :type integer  ; TODO: pointer
                          :accessor secret-view-active))
    (:documentation "Text Input Box control, ask for text, supports secret")))

(defun-pt-num gui-text-input-box claylib/ll:gui-text-input-box
  "Text Input Box control, ask for text"
  (bounds rl-rectangle)
  (title string)
  (message string)
  (buttons string)
  (text string)
  (text-max-size integer)
  (secret-view-active integer))

(defmethod draw-object ((obj gui-text-input-box))
  (setf (slot-value obj '%selected)
        (gui-text-input-box (bounds obj)
                            (title obj)
                            (message obj)
                            (buttons obj)
                            (text obj)
                            (text-max-size obj)
                            (secret-view-active obj))))

(defun make-gui-text-input-box (x y width height text-max-size
                                &rest args &key text title message buttons secret-view-active)
  (declare (ignorable text title message buttons secret-view-active))
  (apply #'make-instance 'gui-text-input-box
         :bounds (make-simple-rec x y width height)
         :text-max-size text-max-size
         args))
