(in-package #:claylib)

(defclass rl-texture ()
  ((%c-struct
    :type claylib/ll:texture
    :initform (autowrap:alloc 'claylib/ll:texture)
    :accessor c-struct)))

(defcreader id rl-texture id texture)
(defcreader width rl-texture width texture)
(defcreader height rl-texture height texture)
(defcreader mipmaps rl-texture mipmaps texture)
(defcreader data-format rl-texture format texture)

(defcwriter id rl-texture id texture integer)
(defcwriter width rl-texture width texture integer)
(defcwriter height rl-texture height texture integer)
(defcwriter mipmaps rl-texture mipmaps texture integer)
(defcwriter data-format rl-texture format texture integer)

(definitializer rl-texture
    (id integer) (width integer) (height integer) (mipmaps integer) (data-format integer))

(default-free rl-texture)
(default-free-c claylib/ll:texture unload-texture t)



(defclass tex ()
  ((%filter :initarg :filter
            :type integer
            :reader filter)
   (%wrap :initarg :wrap
          :type integer
          :reader wrap)
   (%source :initarg :source
            :type rl-rectangle
            :accessor source)
   (%dest :initarg :dest
          :type rl-rectangle
          :accessor dest)
   (%origin :initarg :origin
            :type rl-vector2
            :accessor origin)
   (%rotation :initarg :rot
              :type (or float integer)
              :reader rot)
   (%tint :initarg :tint
          :type rl-color
          :accessor tint)))

(defwriter-float rot text %rotation)



(defclass texture-object (2d-object tex) ())



(defclass texture (rl-texture tex) ())

(defun make-texture (texture-asset x y
                     &rest args &key width height filter wrap origin rot tint source)
  (declare (ignore filter wrap origin rot tint source))
  (load-asset texture-asset)
  (apply #'make-instance 'texture
         :dest (make-instance 'rl-rectangle
                              :x x
                              :y y
                              :width (or width (claylib/ll:texture.width
                                                (c-asset texture-asset)))
                              :height (or height (claylib/ll:texture.height
                                                  (c-asset texture-asset))))
         args))

(defun make-texture (texture-asset source dest
                     &key filter wrap (origin (make-vector2 0 0)) (rot 0.0) (tint +white+))
  (let ((texture (make-instance 'texture
                 :filter filter
                 :wrap wrap
                 :source source
                 :dest dest
                 :origin origin
                 :rot rot
                 :tint tint)))
    (setf (c-struct texture)
          (c-asset (load-asset texture-asset)))
    texture))

(defmethod free ((obj texture))
  (when (slot-boundp obj '%source)
    (free (source obj)))
  (when (slot-boundp obj '%dest)
    (free (dest obj)))
  (free (origin obj))
  (when (next-method-p)
    (call-next-method)))

(defmethod (setf filter) ((value integer) (texture texture))
  (claylib/ll:set-texture-filter (c-struct texture) value)
  (setf (slot-value texture '%filter) value))

(defmethod (setf wrap) ((value integer) (texture texture))
  (claylib/ll:set-texture-wrap (c-struct texture) value)
  (setf (slot-value texture '%wrap) value))

(default-slot-value texture %origin (make-vector2 0 0))
(default-slot-value texture %rotation 0.0)

(defmethod draw-object ((obj texture))
  (claylib/ll:draw-texture-pro (c-struct obj)
                               (c-struct (source obj))
                               (c-struct (dest obj))
                               (c-struct (origin obj))
                               (rot obj)
                               (c-struct (tint obj))))



(defclass rl-render-texture ()
  ((%texture :initarg :texture
             :type texture
             :reader texture)
   (%depth :initarg :depth
           :type texture
           :reader depth)
   (%c-struct
    :type claylib/ll:render-texture
    :initform (autowrap:alloc 'claylib/ll:render-texture)
    :accessor c-struct)))

(defcreader id rl-render-texture id render-texture)

(defcwriter id rl-render-texture id render-texture integer)
(defcwriter-struct texture rl-render-texture texture render-texture texture
  id width height mipmaps data-format)
(defcwriter-struct depth rl-render-texture depth render-texture texture
  id width height mipmaps data-format)

(definitializer rl-render-texture
    (id integer) (texture texture) (depth texture))

(default-free rl-render-texture)
(default-free-c claylib/ll:render-texture unload-render-texture)
