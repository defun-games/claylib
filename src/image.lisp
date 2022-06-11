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



(defclass image (rl-image)
  ((%source :initarg :source
            :type rl-rectangle
            :accessor source)
   (%dest :initarg :dest
          :type rl-rectangle
          :accessor dest)
   (%tint :initarg :tint
          :type rl-color
          :accessor tint)))

(defun make-image (asset source dest &key (tint +white+) (copy-asset nil))
  (let ((img (make-instance 'image
                            :source source
                            :dest dest
                            :tint tint)))
    (setf (c-struct img) (if copy-asset
                             (c-struct (copy-asset-to-object asset))
                             (c-asset asset)))
    img))

(defmethod image-draw (image (obj image))
  (claylib/ll:image-draw (c-struct image)
                         (c-struct obj)
                         (c-struct (source obj))
                         (c-struct (dest obj))
                         (c-struct (tint obj))))

(defmethod image-draw (image (obj pixel))
  (claylib/ll:image-draw-pixel (c-struct image)
                               (truncate (x obj))
                               (truncate (y obj))
                               (c-struct (color obj))))

(defmethod image-draw (image (obj circle))
  (claylib/ll:image-draw-circle (c-struct image)
                                (truncate (x obj))
                                (truncate (y obj))
                                (truncate (radius obj))
                                (c-struct (color obj))))

(defmethod image-draw (image (obj rectangle))
  (claylib/ll:image-draw-rectangle (c-struct image)
                                   (truncate (x obj))
                                   (truncate (y obj))
                                   (truncate (width obj))
                                   (truncate (height obj))
                                   (c-struct (color obj))))

(defmethod image-draw (image (obj text))
  (claylib/ll:image-draw-text-ex (c-struct image)
                                 (c-struct (font obj))
                                 (text obj)
                                 (c-struct (pos obj))
                                 (size obj)
                                 (spacing obj)
                                 (c-struct (color obj))))
