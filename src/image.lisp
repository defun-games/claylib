(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-image (c-struct linkable)
    ()
    (:default-initargs
     :c-ptr (calloc 'claylib/ll:image))))

(defcreader data rl-image data image) ; pointer
(defcreader width rl-image width image)
(defcreader height rl-image height image)
(defcreader mipmaps rl-image mipmaps image)
(defcreader data-format rl-image format image)

(define-print-object rl-image
    (data width height mipmaps data-format))

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

(child-setter image source dest tint)

(define-print-object image
    (source dest tint))

(definitializer image
  :lisp-slots ((%source) (%dest) (%tint))
  :unload (safe-unload-image nil))

(defun make-simple-image (asset &rest args)
  (apply #'make-instance 'image
         :c-ptr (full-copy-image (c-asset asset))
         args))

(defun make-image (asset source dest
                   &rest args &key tint)
  (declare (ignorable tint))
  (apply #'make-instance 'image
         :allow-other-keys t
         :source source
         :dest dest
         :c-ptr (full-copy-image (c-asset asset))
         args))

(defmethod image-draw (image (obj image))
  (claylib/ll:image-draw (c-ptr image)
                         (c-ptr obj)
                         (c-ptr (source obj))
                         (c-ptr (dest obj))
                         (c-ptr (tint obj))))
