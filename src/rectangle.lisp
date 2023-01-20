(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-rectangle (linkable)
    ((%c-struct
      :type claylib/ll:rectangle
      :accessor c-struct))
    (:default-initargs
     :c-struct (autowrap:calloc 'claylib/ll:rectangle))))

(defcreader x rl-rectangle x rectangle)
(defcreader y rl-rectangle y rectangle)
(defcreader width rl-rectangle width rectangle)
(defcreader height rl-rectangle height rectangle)

(define-print-object rl-rectangle
    (x y width height))

(defcwriter x rl-rectangle x rectangle number float)
(defcwriter y rl-rectangle y rectangle number float)
(defcwriter width rl-rectangle width rectangle number float)
(defcwriter height rl-rectangle height rectangle number float)

(definitializer rl-rectangle
  :pt-accessors ((x number float)
                 (y number float)
                 (width number float)
                 (height number float)))

(defun make-simple-rec (x y width height)
  "Make an RL-RECTANGLE for non-drawable uses such as defining a source or dest rectangle."
  (make-instance 'rl-rectangle :x x :y y :width width :height height))



(defconstant +foreign-rectangle-size+ (autowrap:sizeof 'claylib/ll:rectangle))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-rectangles (rl-sequence)
    ((%cl-array :type (array rl-rectangle 1)))))

(define-print-object rl-rectangles
    ())

(defmethod make-rl-*-array ((c-struct claylib/ll:rectangle) num)
  (let ((contents (loop for i below num
                        for rect = (make-instance 'rl-rectangle)
                        do (setf (slot-value rect '%c-struct)
                                 (autowrap:c-aref c-struct i 'claylib/ll:rectangle))
                        collect rect)))
    (make-array num
                :element-type 'rl-rectangle
                :initial-contents contents)))

(defmethod (setf sequences:elt) (value (sequence rl-rectangles) index)
  (check-type value rl-rectangle)
  (cffi:foreign-funcall "memcpy"
                        :pointer (autowrap:ptr (c-struct (elt sequence index)))
                        :pointer (autowrap:ptr (c-struct value))
                        :int +foreign-rectangle-size+
                        :void))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rectangle (rl-rectangle 2d-shape)
    ((%position :initarg :origin
                :accessor origin)
     (%rotation :initarg :rot
                :type number
                :reader rot)
     (%thickness :initarg :thickness
                 :type number
                 :reader thickness)
     ;; TODO support custom vertex colors ('EX)
     (%gradient-style :initarg :gradient-style
                      :type keyword
                      :accessor gradient-style
                      :documentation
                      "Whether to draw a gradient horizontally or vertically. Must be :H or :V."))
    (:default-initargs
     :rot 0.0
     :thickness 1.0
     :origin (make-vector2 0 0))))

(define-print-object rectangle
    (origin rot thickness gradient-style))

(defwriter-float rot rectangle %rotation)
(defwriter-float thickness rectangle)

(definitializer rectangle
  :lisp-slots ((%position)
               (%rotation t)
               (%thickness t)
               (%gradient-style)))

(defun make-rectangle (x y width height color
                       &rest args &key color2 filled rot thickness origin gradient-style)
  (declare (ignorable color2 filled rot thickness origin gradient-style))
  (apply #'make-instance 'rectangle
         :x x
         :y y
         :width width
         :height height
         :color color
         args))

(defun make-rectangle-from-vecs (pos size color
                                 &rest args &key color2 filled rotation thickness origin gradient-style)
  (declare (ignorable color2 filled rotation thickness origin gradient-style))
  (apply #'make-instance 'rectangle
         :x (x pos)
         :y (y pos)
         :width (x size)
         :height (y size)
         :color color
         args))

(defmethod draw-object ((obj rectangle))
  (cond
    ((and (filled obj) (slot-boundp obj '%color2) (gradient-style obj))
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
