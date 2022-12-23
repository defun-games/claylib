(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-text-input-box (gui-message-box text-label)
    ((%text-max-size :initarg :text-max-size
                     :type integer
                     :accessor text-max-size)
     (%secret-view-active :initarg :secret-view-active
                          :type cffi:foreign-pointer))
    (:default-initargs
     :secret-view-active nil)
    (:documentation "Text Input Box control, ask for text, supports secret")))

(defmethod secret-view-active ((input-box gui-text-input-box))
  (ecase (plus-c:c-ref (slot-value input-box '%secret-view-active) :int)
    (0 nil)
    (1 t)))

(defmethod (setf secret-view-active) (value (input-box gui-text-input-box))
  (if (slot-value input-box '%secret-view-active)
      (setf (plus-c:c-ref (slot-value input-box '%secret-view-active) :int)
            (if value 1 0))
      (let ((ptr (autowrap:calloc :int)))
        (setf (plus-c:c-ref ptr :int) (if value 1 0)
              (slot-value input-box '%secret-view-active) ptr))))

(defmethod initialize-instance :after ((input-box gui-text-input-box)
                                       &key secret-view-active &allow-other-keys)
  (setf (secret-view-active input-box) secret-view-active)
  input-box)

(defun-pt-num gui-text-input-box claylib/ll:gui-text-input-box
  "Text Input Box control, ask for text"
  (bounds rl-rectangle)
  (title string)
  (message string)
  (buttons string)
  (text string)
  (text-max-size integer)
  (secret-view-active cffi:foreign-pointer))

(defmethod draw-object ((obj gui-text-input-box))
  (setf (slot-value obj '%selected)
        (gui-text-input-box (bounds obj)
                            (title obj)
                            (message obj)
                            (buttons obj)
                            (text obj)
                            (text-max-size obj)
                            (slot-value obj '%secret-view-active))))

(defun make-gui-text-input-box (x y width height text-max-size
                                &rest args &key text title message buttons secret-view-active)
  (declare (ignorable text title message buttons secret-view-active))
  (apply #'make-instance 'gui-text-input-box
         :bounds (make-simple-rec x y width height)
         :text-max-size text-max-size
         args))
