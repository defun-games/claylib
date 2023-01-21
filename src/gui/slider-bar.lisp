(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-slider-bar (gui-object value-bar) ()
    (:documentation "Slider Bar control, sets VALUE to selected value")))

(define-print-object gui-slider-bar
    ())

(defun-pt-num gui-slider-bar claylib/ll:gui-slider-bar
  "Slider Bar control, returns selected value"
  (bounds rl-rectangle)
  (text-left string)
  (text-right string)
  (value number float)
  (min-value number float)
  (max-value number float))

(defmethod draw-object ((obj gui-slider-bar))
  (setf (value obj)
        (gui-slider-bar (bounds obj)
                        (text-left obj)
                        (text-right obj)
                        (value obj)
                        (min-value obj)
                        (max-value obj))))

(defun make-gui-slider-bar (x y width height min-value max-value
                            &rest args &key text-left text-right value)
  (declare (ignorable text-left text-right))
  (apply #'make-instance 'gui-slider-bar
         :bounds (make-simple-rec x y width height)
         :min-value min-value
         :max-value max-value
         :value (or value (/ (+ min-value max-value) 2))
         args))
