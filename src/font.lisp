(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-glyph-info (c-struct linkable)
    ((%image :initarg :image
             :type rl-image
             :reader image))
    (:default-initargs
     :c-ptr (calloc 'claylib/ll:glyph-info))))

(defcreader value rl-glyph-info value glyph-info)
(defcreader offset-x rl-glyph-info offset-x glyph-info)
(defcreader offset-y rl-glyph-info offset-y glyph-info)
(defcreader advance-x rl-glyph-info advance-x glyph-info)

(define-print-object rl-glyph-info
    (image value offset-x offset-y advance-x))

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

(define-print-object glyph-info
    ())


(defconstant +foreign-glyph-info-size+ (cffi:foreign-type-size 'claylib/ll:glyph-info))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-glyphs (rl-sequence)
    ((%cl-array :type (array rl-glyph-info 1)))))

(define-print-object rl-glyphs
    ())

(defun make-rl-glyph-info-array (c-ptr num &optional finalize)
  (let ((contents (loop for i below num
                        for c-elt = (cffi:mem-aref c-ptr 'claylib/ll:glyph-info i)
                        for glyph = (make-instance 'rl-glyph-info :c-ptr c-elt
                                                                  :finalize (when finalize (= i 0)))
                        do (setf (slot-value glyph '%image)
                                 (make-instance 'rl-image
                                                :c-ptr (field-value c-elt 'glyph-info 'image)
                                                :finalize nil))
                        collect glyph)))
    (make-array num
                :element-type 'rl-glyph-info
                :initial-contents contents)))

(defmethod (setf sequences:elt) (value (sequence rl-glyphs) index)
  (check-type value rl-glyph-info)
  (cffi:foreign-funcall "memcpy"
                        :pointer (c-ptr (elt sequence index))
                        :pointer (c-ptr value)
                        :int +foreign-glyph-info-size+
                        :void))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-font (c-struct linkable)
    ((%texture :initarg :texture
               :type rl-texture
               :reader texture)
     (%recs :initarg :recs
            :type rl-rectangles
            :accessor recs)
     (%glyphs :initarg :glyphs
              :type rl-glyphs
              :accessor glyphs))
    (:default-initargs
     :c-ptr (calloc 'claylib/ll:font))))

(defcreader size rl-font base-size font)
(defcreader glyph-count rl-font glyph-count font)
(defcreader glyph-padding rl-font glyph-padding font)

(define-print-object rl-font
    (texture recs glyphs size glyph-count glyph-padding))

(child-setter rl-font recs glyphs)

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
                 (glyph-padding integer))
  :unload (safe-unload-font t))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass font (rl-font) ()))

(define-print-object font
    ())

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

(define-print-object default-font
    ())

(defmethod initialize-instance :around ((font default-font) &rest initargs &key &allow-other-keys)
  (setf (c-ptr font)
        (make-instance 'c-ptr :c-ptr (getf initargs :c-ptr)))
  (claylib/ll:get-font-default (c-ptr font))
  font)

(defparameter +default-font+ nil)

(defun load-font-default ()
  (make-instance 'default-font))
