(in-package #:claylib)

(defclass text (2d-object)
  ((%text :initarg :text
          :type string
          :accessor text)
   (%font :initarg :font
          :type rl-font
          :accessor font)
   (%font-size :initarg :size
               :type (or integer float)
               :reader size)
   (%spacing :initarg :spacing
             :type (or integer float)
             :reader spacing)
   (%color :initarg :color
           :type rl-color
           :accessor color)))

(defwriter-float size text %font-size)
(defwriter-float spacing text)

(definitializer text
    (text string) (font rl-font nil) (size number float) (spacing number float) (color rl-color nil))

(default-slot-value text %font +default-font+)
(default-slot-value text %font-size (float (size +default-font+)))
(default-slot-value text %spacing 1.0)
(default-slot-value text %color +gray+)

(defun make-text (text x y &rest args &key size color spacing font)
  (declare (ignore size color spacing font))
  (apply #'make-instance 'text
         :text text
         :pos (make-vector2 x y)
         args))

(defmethod draw-object ((obj text))
  (claylib/ll:draw-text-ex (c-struct (font obj))
                           (text obj)
                           (c-struct (pos obj))
                           (size obj)
                           (spacing obj)
                           (c-struct (color obj))))

(defmethod image-draw (image (obj text))
  (claylib/ll:image-draw-text-ex (c-struct image)
                                 (c-struct (font obj))
                                 (text obj)
                                 (c-struct (pos obj))
                                 (size obj)
                                 (spacing obj)
                                 (c-struct (color obj))))
