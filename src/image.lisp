(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-image ()
    ((%c-struct
      :type claylib/ll:image
      :initform (autowrap:alloc 'claylib/ll:image)
      :accessor c-struct))))

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

(default-free rl-image)
(default-free-c claylib/ll:image)
;; TODO: For some reason UNLOAD-IMAGE is broken now and I'm too tired to figure out why.
;(default-free-c claylib/ll:image unload-image)



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
            :accessor tint))))

(definitializer image
  :lisp-slots ((%source) (%dest) (%tint)))

(default-slot-value image %tint +white+)

(default-free image %source %dest %tint)

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
