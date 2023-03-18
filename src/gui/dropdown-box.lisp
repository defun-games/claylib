(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-dropdown-box (gui-object text-label pressable editable)
    ((%active :initarg :active
              :type cffi:foreign-pointer))
    (:default-initargs
     :active 0)
    (:documentation "Dropdown Box control, sets PRESSED when clicked")))

(child-setter gui-dropdown-box active)

(define-print-object gui-dropdown-box
    ())

(defmethod active ((dropdown gui-dropdown-box))
  (cffi:mem-ref (slot-value dropdown '%active) :int))

(defmethod (setf active) (value (dropdown gui-dropdown-box))
  (setf (cffi:mem-ref (slot-value dropdown '%active) :int) value))

(defmethod initialize-instance :after ((dropdown gui-dropdown-box)
                                       &key active &allow-other-keys)
  (let ((ptr (calloc :int)))
    (setf (cffi:mem-ref ptr :int) active
          (slot-value dropdown '%active) ptr)
    dropdown))

(defun-pt-bool gui-dropdown-box claylib/ll:gui-dropdown-box
  "Dropdown Box control, returns selected item"
  (bounds rl-rectangle)
  (text string)
  (active cffi:foreign-pointer)
  (edit-mode boolean))

(defmethod draw-object ((obj gui-dropdown-box))
  (setf (slot-value obj '%pressed)
        (gui-dropdown-box (bounds obj)
                          (text obj)
                          (slot-value obj '%active)
                          (edit-mode obj))))

(static-draw draw-gui-dropdown-box-object gui-dropdown-box)

(defun make-gui-dropdown-box (x y width height &rest args &key text active edit-mode)
  (declare (ignorable text active edit-mode))
  (apply #'make-instance 'gui-dropdown-box
         :bounds (make-simple-rec x y width height)
         args))
