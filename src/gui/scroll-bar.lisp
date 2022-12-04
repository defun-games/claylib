(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-scroll-bar (gui-object int-values) ()
    (:documentation "Scroll Bar control (Raylib internal only?)")))

(defun-pt-num gui-scroll-bar claylib/ll:gui-scroll-bar
  "Scroll Bar control"
  (bounds rl-rectangle)
  (value integer)
  (min-value integer)
  (max-value integer))

(defmethod draw-object ((obj gui-scroll-bar))
  (setf (value obj)
        (gui-scroll-bar (bounds obj)
                        (value obj)
                        (min-value obj)
                        (max-value obj))))

(defun make-gui-scroll-bar (x y width height min-value max-value &rest args &key value)
  (apply #'make-instance 'gui-scroll-bar
         :bounds (make-simple-rec x y width height)
         :min-value min-value
         :max-value max-value
         :value (or value min-value)
         args))
