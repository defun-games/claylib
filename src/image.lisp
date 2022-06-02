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
