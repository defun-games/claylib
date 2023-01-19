(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-slider (gui-object value-bar) ()
    (:documentation "Slider control, sets VALUE to selected value")))

(define-print-object gui-slider
    ())

(defun-pt-num gui-slider claylib/ll:gui-slider
  "Slider control, returns selected value"
  (bounds rl-rectangle)
  (text-left string)
  (text-right string)
  (value number float)
  (min-value number float)
  (max-value number float))

(defmethod draw-object ((obj gui-slider))
  (setf (value obj)
        (gui-slider (bounds obj)
                    (text-left obj)
                    (text-right obj)
                    (value obj)
                    (min-value obj)
                    (max-value obj))))

(defun make-gui-slider (x y width height min-value max-value
                        &rest args &key text-left text-right value)
  (declare (ignorable text-left text-right))
  (apply #'make-instance 'gui-slider
         :bounds (make-simple-rec x y width height)
         :min-value min-value
         :max-value max-value
         :value (or value (/ (+ min-value max-value) 2))
         args))
