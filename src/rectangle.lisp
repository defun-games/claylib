(in-package #:claylib)

(defclass rl-rectangle ()
  ((%c-struct
    :type claylib/ll:rectangle
    :initform (autowrap:alloc 'claylib/ll:rectangle)
    :accessor c-struct)))

(defcreader x rl-rectangle x rectangle)
(defcreader y rl-rectangle y rectangle)
(defcreader width rl-rectangle width rectangle)
(defcreader height rl-rectangle height rectangle)

(defcwriter x rl-rectangle x rectangle number float)
(defcwriter y rl-rectangle y rectangle number float)
(defcwriter width rl-rectangle width rectangle number float)
(defcwriter height rl-rectangle height rectangle number float)

(definitializer rl-rectangle
    (x number float) (y number float) (width number float) (height number float))

(default-free rl-rectangle)
(default-free-c claylib/ll:rectangle)



(defclass rectangle (rl-rectangle 2d-shape)
  ((%position :initarg :origin
              :accessor origin)
   (%rotation :initarg :rot
              :type (or integer float)
              :reader rot)
   (%thickness :initarg :thickness
               :type (or integer float)
               :accessor thickness)))

(defwriter-float rot rectangle %rotation)
(defwriter-float thickness rectangle)

(definitializer rectangle
    (rot number float 0.0) (thickness number float 1.0) (origin rl-vector2 nil (make-vector2 0 0)))

(default-free rectangle)

(defun make-rectangle (x y width height color
                       &key (filled t) (rotation 0) (thickness 1)
                         (origin (make-vector2 0 0)))
  (make-instance 'rectangle :x x :y y :width width :height height :color color
                 :filled filled :rot rotation :thickness thickness :origin origin))

(defun make-rectangle-from-vecs (pos size color
                                 &key (filled t) (rotation 0) (thickness 1)
                                   (origin (make-vector2 0 0)))
  (make-instance 'rectangle :x (x pos) :y (y pos) :width (x size) :height (y size) :color color
                 :filled filled :rot rotation :thickness thickness :origin origin))

(defmethod draw-object ((obj rectangle))
  (if (filled obj)
      (claylib/ll:draw-rectangle-pro (c-struct obj)
                                     (c-struct (origin obj))
                                     (rot obj)
                                     (c-struct (color obj)))
      (claylib/ll:draw-rectangle-lines-ex (c-struct obj)
                                          (thickness obj)
                                          (c-struct (color obj)))))
