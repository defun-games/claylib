(in-package #:claylib)

(defclass rl-image ()
  ((%c-struct
    :type claylib/ll:image
    :initform (autowrap:alloc 'claylib/ll:image)
    :accessor c-struct)))

(defcreader data rl-image data image) ; pointer
(defcreader width rl-image width image)
(defcreader height rl-image height image)
(defcreader mipmaps rl-image mipmaps image)
(defcreader data-format rl-image format image)

(defcwriter data rl-image data image) ; pointer
(defcwriter width rl-image width image integer)
(defcwriter height rl-image height image integer)
(defcwriter mipmaps rl-image mipmaps image integer)
(defcwriter data-format rl-image format image integer)

(definitializer rl-image
    (width integer) (height integer) (mipmaps integer) (data-format integer))

(default-free rl-image)
(default-free-c claylib/ll:image unload-image)



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
(default-free-c claylib/ll:texture unload-texture)



(defclass texture (rl-texture game-asset)
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

(defwriter-float rot texture %rotation)

(defmethod load-asset ((asset texture) &key force-reload)
  "Return a pointer to the texture OBJ.

When the texture has no pointer yet or the caller has requested a FORCE-RELOAD, load the texture
from ASSETS's path and give it a pointer."
  (if (or force-reload (null (ptr asset)))
      (c-let ((c claylib/ll:texture))
        (claylib/ll:load-texture c (path asset))
        (setf (ptr asset) (autowrap:ptr c)))
      (ptr asset)))

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
