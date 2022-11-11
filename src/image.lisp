(in-package #:claylib)

(default-unload claylib/ll:image unload-image)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-image ()
    ((%c-struct
      :type claylib/ll:image
      :accessor c-struct))
    (:default-initargs
     :c-struct (autowrap:calloc 'claylib/ll:image))))

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
  :pt-accessors (; TODO: data?
                 (width integer)
                 (height integer)
                 (mipmaps integer)
                 (data-format integer)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass image (rl-image)
    ((%source :initarg :source
              :type rl-rectangle
              :accessor source)
     (%dest :initarg :dest
            :type rl-rectangle
            :accessor dest)
     (%tint :initarg :tint
            :type rl-color
            :accessor tint))
    (:default-initargs
     :tint +white+)))

(definitializer image
  :lisp-slots ((%source) (%dest) (%tint)))

(defun make-image (asset source dest
                   &rest args &key tint (copy-asset nil))
  (declare (ignorable tint))
  (let ((img (apply #'make-instance 'image
                    :allow-other-keys t
                    :source source
                    :dest dest
                    args)))
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
