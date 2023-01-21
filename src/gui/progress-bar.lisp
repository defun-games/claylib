(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-progress-bar (gui-object value-bar) ()
    (:documentation "Progress Bar control, sets VALUE to current progress value")))

(define-print-object gui-progress-bar
    ())

(defun-pt-num gui-progress-bar claylib/ll:gui-progress-bar
  "Progress Bar control, shows current progress value"
  (bounds rl-rectangle)
  (text-left string)
  (text-right string)
  (value number float)
  (min-value number float)
  (max-value number float))

(defmethod draw-object ((obj gui-progress-bar))
  (setf (value obj)
        (gui-progress-bar (bounds obj)
                          (text-left obj)
                          (text-right obj)
                          (value obj)
                          (min-value obj)
                          (max-value obj))))

(defun make-gui-progress-bar (x y width height min-value max-value
                              &rest args &key text-left text-right value)
  (declare (ignorable text-left text-right))
  (apply #'make-instance 'gui-slider-bar
         :bounds (make-simple-rec x y width height)
         :min-value min-value
         :max-value max-value
         :value (or value min-value)
         args))
