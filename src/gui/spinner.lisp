(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-spinner (gui-object text-label pressable editable int-values) ()
    (:documentation "Spinner control, sets PRESSED when clicked")))

(define-print-object gui-spinner
    ())

(defun gui-spinner (bounds text value min-value max-value edit-mode)
  "Spinner control, returns selected value"
  (check-type bounds rl-rectangle)
  (check-type text string)
  (check-type value integer)
  (check-type min-value integer)
  (check-type max-value integer)
  (check-type edit-mode boolean)
  (c-with ((val :int :value value))
    (if (= 0 (claylib/wrap:gui-spinner (c-struct bounds)
                                       text
                                       (val &)
                                       min-value
                                       max-value
                                       (if edit-mode 1 0)))
        nil
        t)))

(defmethod draw-object ((obj gui-spinner))
  (setf (slot-value obj '%pressed)
        (gui-spinner (bounds obj)
                     (text obj)
                     (value obj)
                     (min-value obj)
                     (max-value obj)
                     (edit-mode obj))))

(defun make-gui-spinner (x y width height min-value max-value &rest args &key text value edit-mode)
  (declare (ignorable text value edit-mode))
  (apply #'make-instance 'gui-spinner
         :bounds (make-simple-rec x y width height)
         :min-value min-value
         :max-value max-value
         args))
