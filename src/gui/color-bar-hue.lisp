(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-color-bar-hue (gui-object text-label)
    ((%value :initarg :value
             :type number
             :reader value))
    (:default-initargs
     :value 0.0)
    (:documentation "Color Bar Hue control, sets VALUE to hue value")))

(defwriter-float value gui-color-bar-hue)

(define-print-object gui-color-bar-hue
    (value))

(definitializer gui-color-bar-hue
  :lisp-slots ((%value nil float)))

(defun-pt-num gui-color-bar-hue claylib/ll:gui-color-bar-hue
  "Color Bar Hue control"
  (bounds rl-rectangle)
  (text string)
  (value number float))

(defmethod draw-object ((obj gui-color-bar-hue))
  (setf (value obj)
        (gui-color-bar-hue (bounds obj)
                           (text obj)
                           (value obj))))

(defun make-gui-color-bar-hue (x y width height &rest args &key text value)
  (declare (ignorable text value))
  (apply #'make-instance 'gui-color-bar-hue
         :bounds (make-simple-rec x y width height)
         args))
