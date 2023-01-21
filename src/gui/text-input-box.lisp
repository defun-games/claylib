(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-text-input-box (gui-message-box text-box)
    ((%secret-view-active :initarg :secret-view-active
                          :type cffi:foreign-pointer))
    (:default-initargs
     :secret-view-active nil)
    (:documentation "Text Input Box control, ask for text, supports secret")))

(define-print-object gui-text-input-box
    ())

(defmethod secret-view-active ((input-box gui-text-input-box))
  (ecase (plus-c:c-ref (slot-value input-box '%secret-view-active) :int)
    (0 nil)
    (1 t)))

(defmethod (setf secret-view-active) (value (input-box gui-text-input-box))
  (setf (plus-c:c-ref (slot-value input-box '%secret-view-active) :int)
        (if value 1 0)))

(defmethod initialize-instance :after ((input-box gui-text-input-box)
                                       &key secret-view-active &allow-other-keys)
  (let ((ptr (autowrap:calloc :int)))
    (setf (plus-c:c-ref ptr :int) (if secret-view-active 1 0)
          (slot-value input-box '%secret-view-active) ptr)
    input-box))

(defun-pt-num gui-text-input-box claylib/ll:gui-text-input-box
  "Text Input Box control, ask for text"
  (bounds rl-rectangle)
  (title string)
  (message string)
  (buttons string)
  (text cffi:foreign-pointer)
  (text-size integer)
  (secret-view-active cffi:foreign-pointer))

(defmethod draw-object ((obj gui-text-input-box))
  (setf (slot-value obj '%selected)
        (gui-text-input-box (bounds obj)
                            (title obj)
                            (message obj)
                            (buttons obj)
                            (slot-value obj '%text)
                            (text-size obj)
                            (slot-value obj '%secret-view-active))))

(defun make-gui-text-input-box (x y width height text-size
                                &rest args &key text title message buttons secret-view-active)
  (declare (ignorable text title message buttons secret-view-active))
  (apply #'make-instance 'gui-text-input-box
         :bounds (make-simple-rec x y width height)
         :text-size text-size
         args))
