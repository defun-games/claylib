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

(defun make-simple-rec (x y width height)
  "Make an RL-RECTANGLE for non-drawable uses such as defining a source or dest rectangle."
  (make-instance 'rl-rectangle :x x :y y :width width :height height))



(defclass rectangle (rl-rectangle 2d-shape)
  ((%position :initarg :origin
              :accessor origin)
   (%rotation :initarg :rot
              :type (or integer float)
              :reader rot)
   (%thickness :initarg :thickness
               :type (or integer float)
               :accessor thickness)
   ;; TODO support custom vertex colors ('EX)
   (%gradient-style :initarg :gradient-style
                    :initform nil
                    :type (or keyword null)
                    :accessor gradient-style
                    :documentation
                    "Whether to draw a gradient horizontally or vertically. Must be :H or :V.")))

(defwriter-float rot rectangle %rotation)
(defwriter-float thickness rectangle)

(definitializer rectangle
    (rot number float 0.0) (thickness number float 1.0) (origin rl-vector2 nil (make-vector2 0 0)))

(default-free rectangle)

(defun make-rectangle (x y width height color
                       &key (color2 nil) (filled t) (rotation 0) (thickness 1)
                         (origin (make-vector2 0 0)) (gradient-style nil))
  (make-instance 'rectangle :x x :y y :width width :height height :color color :color2 color2
                            :filled filled :rot rotation :thickness thickness :origin origin
                            :gradient-style gradient-style))

(defun make-rectangle-from-vecs (pos size color
                                 &key (filled t) (rotation 0) (thickness 1)
                                   (origin (make-vector2 0 0)))
  (make-instance 'rectangle :x (x pos) :y (y pos) :width (x size) :height (y size) :color color
                 :filled filled :rot rotation :thickness thickness :origin origin))

(defmethod draw-object ((obj rectangle))
  (cond
    ((and (filled obj) (color2 obj) (gradient-style obj))
     (funcall (case (gradient-style obj)
                (:v #'claylib/ll:draw-rectangle-gradient-v)
                (:h #'claylib/ll:draw-rectangle-gradient-h))
              (truncate (x obj))
              (truncate (y obj))
              (truncate (width obj))
              (truncate (height obj))
              (c-struct (color obj))
              (c-struct (color2 obj))))
    ((filled obj)
     (claylib/ll:draw-rectangle-pro (c-struct obj)
                                    (c-struct (origin obj))
                                    (rot obj)
                                    (c-struct (color obj))))
    (t
     (claylib/ll:draw-rectangle-lines-ex (c-struct obj)
                                         (thickness obj)
                                         (c-struct (color obj))))))

(defmethod image-draw (image (obj rectangle))
  (claylib/ll:image-draw-rectangle (c-struct image)
                                   (truncate (x obj))
                                   (truncate (y obj))
                                   (truncate (width obj))
                                   (truncate (height obj))
                                   (c-struct (color obj))))
