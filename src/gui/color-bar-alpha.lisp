(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-color-bar-alpha (gui-object text-label)
    ((%alpha :initarg :alpha
             :type number
             :reader alpha))
    (:default-initargs
     :alpha 1.0)
    (:documentation "Color Bar Alpha control, sets ALPHA to alpha value")))

(define-print-object gui-color-bar-alpha
  (alpha))

(defwriter-float alpha gui-color-bar-alpha)

(definitializer gui-color-bar-alpha
  :lisp-slots ((%alpha nil float)))

(defun-pt-num gui-color-bar-alpha claylib/ll:gui-color-bar-alpha
  "Color Bar Alpha control"
  (bounds rl-rectangle)
  (text string)
  (alpha number float))

(defmethod draw-object ((obj gui-color-bar-alpha))
  (setf (alpha obj)
        (gui-color-bar-alpha (bounds obj)
                             (text obj)
                             (alpha obj))))

(defun make-gui-color-bar-alpha (x y width height &rest args &key text alpha)
  (declare (ignorable text alpha))
  (apply #'make-instance 'gui-color-bar-alpha
         :bounds (make-simple-rec x y width height)
         args))
