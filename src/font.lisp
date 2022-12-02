(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-glyph-info ()
    ((%image :initarg :image
             :type rl-image
             :reader image)
     (%c-struct
      :type claylib/ll:glyph-info
      :accessor c-struct))
    (:default-initargs
     :c-struct (autowrap:calloc 'claylib/ll:glyph-info))))

(defcreader value rl-glyph-info value glyph-info)
(defcreader offset-x rl-glyph-info offset-x glyph-info)
(defcreader offset-y rl-glyph-info offset-y glyph-info)
(defcreader advance-x rl-glyph-info advance-x glyph-info)

(defcwriter value rl-glyph-info value glyph-info integer)
(defcwriter offset-x rl-glyph-info offset-x glyph-info integer)
(defcwriter offset-y rl-glyph-info offset-y glyph-info integer)
(defcwriter advance-x rl-glyph-info advance-x glyph-info integer)
(defcwriter-struct image rl-glyph-info image glyph-info image
  data width height mipmaps data-format)

(definitializer rl-glyph-info
  :struct-slots ((%image))
  :pt-accessors ((value integer)
                 (offset-x integer)
                 (offset-y integer)
                 (advance-x integer)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass glyph-info (rl-glyph-info) ()))



(defconstant +foreign-glyph-info-size+ (autowrap:sizeof 'claylib/ll:glyph-info))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-glyphs (rl-sequence)
    ((%cl-array :type (array rl-glyph-info 1)))))

(defmethod make-rl-*-array ((c-struct claylib/ll:glyph-info) num)
  (let ((contents (loop for i below num
                        for glyph = (make-instance 'rl-glyph-info)
                        for c-elt = (autowrap:c-aref c-struct i 'claylib/ll:glyph-info)
                        do (setf (slot-value glyph '%c-struct)
                                 c-elt

                                 (slot-value glyph '%image)
                                 (let ((img (make-instance 'rl-image)))
                                   (setf (c-struct img) (glyph-info.image c-elt))
                                   img))
                        collect glyph)))
    (make-array num
                :element-type 'rl-glyph-info
                :initial-contents contents)))

(defmethod (setf sequences:elt) (value (sequence rl-glyphs) index)
  (check-type value rl-glyph-info)
  (cffi:foreign-funcall "memcpy"
                        :pointer (autowrap:ptr (c-struct (elt sequence index)))
                        :pointer (autowrap:ptr (c-struct value))
                        :int +foreign-glyph-info-size+
                        :void))



(default-unload claylib/ll:font unload-font t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-font ()
    ((%texture :initarg :texture
               :type rl-texture
               :reader texture)
     (%recs :initarg :recs
            :type rl-rectangles
            :accessor recs)
     (%glyphs :initarg :glyphs
              :type rl-glyphs
              :accessor glyphs)
     (%c-struct
      :type claylib/ll:font
      :accessor c-struct))
    (:default-initargs
     :c-struct (autowrap:calloc 'claylib/ll:font))))

(defcreader size rl-font base-size font)
(defcreader glyph-count rl-font glyph-count font)
(defcreader glyph-padding rl-font glyph-padding font)

(defcwriter size rl-font base-size font integer)
(defcwriter glyph-count rl-font glyph-count font integer)
(defcwriter glyph-padding rl-font glyph-padding font integer)
(defcwriter-struct texture rl-font texture font texture
  id width height mipmaps data-format)

(definitializer rl-font
  :lisp-slots ((%recs) (%glyphs))
  :struct-slots ((%texture))
  :pt-accessors ((size integer)
                 (glyph-count integer)
                 (glyph-padding integer)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass font (rl-font) ()))

(defun make-font (asset-or-path
                  &rest args
                  &key size chars glyph-count glyph-padding texture recs glyphs (load-now nil))
  (declare (ignorable size chars glyph-count glyph-padding texture recs glyphs load-now))
  (let ((asset (if (typep asset-or-path asset-or-path)
                   asset-or-path
                   (apply #'make-font-asset asset-or-path args))))
    (apply #'make-instance 'font
           :size (or size (size asset))
           :glyph-count (or glyph-count (glyph-count asset))
           args)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass default-font (font) ()))

(defmethod initialize-instance :around ((font default-font) &rest initargs &key &allow-other-keys)
  (setf (c-struct font) (getf initargs :c-struct))
  (claylib/ll:get-font-default (c-struct font))
  font)

(defparameter +default-font+ nil)

(defun load-font-default ()
  (make-instance 'default-font))
