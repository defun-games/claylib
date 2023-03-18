(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-icon (gui-color linkable)
    ((%icon-id :initarg :icon-id
               :type integer
               :accessor icon-id)
     (%x :initarg :x
         :type integer
         :accessor x)
     (%y :initarg :y
         :type integer
         :accessor y)
     (%pixel-size :initarg :pixel-size
                  :type integer
                  :accessor pixel-size))))

(child-setter gui-icon icon-id x y pixel-size)

(define-print-object gui-icon
    (icon-id x y pixel-size))

(defun-pt-void gui-draw-icon claylib/ll:gui-draw-icon
  "Draw a gui icon"
  (icon-id integer)
  (pos-x integer)
  (pos-y integer)
  (pixel-size integer)
  (color rl-color))

(defmethod draw-object ((obj gui-icon))
  (gui-draw-icon (icon-id obj)
                 (x obj)
                 (y obj)
                 (pixel-size obj)
                 (color obj)))

(static-draw draw-gui-icon-object gui-icon)

(defun make-gui-icon (icon-id x y pixel-size color)
  (make-instance 'gui-icon
                 :icon-id icon-id
                 :x x
                 :y y
                 :pixel-size pixel-size
                 :color color))
