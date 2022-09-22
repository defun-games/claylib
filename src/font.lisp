(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-glyph-info ()
    ((%image :initarg :image
             :type rl-image
             :reader image)
     (%c-struct
      :type claylib/ll:glyph-info
      :initform (autowrap:calloc 'claylib/ll:glyph-info)
      :accessor c-struct))))

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

(defmethod sync-children ((obj rl-glyph-info))
  (unless (eq (c-struct (image obj))
              (glyph-info.image (c-struct obj)))
    (free-later (c-struct (image obj)))
    (setf (c-struct (image obj))
          (glyph-info.image (c-struct obj)))))

(definitializer rl-glyph-info
  :struct-slots ((%image))
  :pt-accessors ((value integer)
                 (offset-x integer)
                 (offset-y integer)
                 (advance-x integer)))

(default-free rl-glyph-info %image)
;; TODO: I think UNLOAD-FONT-DATA should be used here somehow.
(default-free-c claylib/ll:glyph-info)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-font ()
    ((%texture :initarg :texture
               :type rl-texture
               :reader texture)
     (%recs :initarg :recs
            :type rl-rectangle  ; TODO: array/pointer
            :reader recs)
     (%glyphs :initarg :glyphs
              :type rl-glyph-info  ; TODO: array/pointer
              :reader glyphs)
     (%c-struct
      :type claylib/ll:font
      :initform (autowrap:calloc 'claylib/ll:font)
      :accessor c-struct))))

(defcreader size rl-font base-size font)
(defcreader glyph-count rl-font glyph-count font)
(defcreader glyph-padding rl-font glyph-padding font)

(defcwriter size rl-font base-size font integer)
(defcwriter glyph-count rl-font glyph-count font integer)
(defcwriter glyph-padding rl-font glyph-padding font integer)
(defcwriter-struct texture rl-font texture font texture
  id width height mipmaps data-format)
(defcwriter-struct recs rl-font recs font rectangle  ; TODO: array/pointer
  x y width height)
(defcwriter-struct glyphs rl-font glyphs font glyph-info  ; TODO: array/pointer
  value offset-x offset-y advance-x)

(defmethod sync-children ((obj rl-font))
  (flet ((i0 (array type)
           (autowrap:c-aref array 0 type)))
    (unless (eq (c-struct (texture obj))
                (font.texture (c-struct obj)))
      (free-later (c-struct (texture obj)))
      (setf (c-struct (texture obj))
            (font.texture (c-struct obj))))
    (unless (eq (c-struct (recs obj))
                (i0 (font.recs (c-struct obj)) 'claylib/ll:rectangle))
      (free-later (c-struct (recs obj)))
      (setf (c-struct (recs obj))
            (i0 (font.recs (c-struct obj)) 'claylib/ll:rectangle)))
    (unless (eq (c-struct (glyphs obj))
                (i0 (font.glyphs (c-struct obj)) 'claylib/ll:glyph-info))
      (free-later (c-struct (glyphs obj)))
      (setf (c-struct (glyphs obj))
            (i0 (font.glyphs (c-struct obj)) 'claylib/ll:glyph-info))))
  (sync-children (glyphs obj)))

(definitializer rl-font
  :struct-slots ((%texture) (%recs) (%glyphs))
  :pt-accessors ((size integer)
                 (glyph-count integer)
                 (glyph-padding integer)))

(default-free rl-font %texture %recs %glyphs)
(default-free-c claylib/ll:font unload-font)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass font (rl-font game-asset)
    ((%chars :initarg :chars
             :type integer
             :accessor chars))))

(defun make-font (path
                  &key (size 10) (chars 0) (glyph-count 224) (glyph-padding 0) texture recs glyphs)
  (make-instance 'font
                 :path path
                 :size size
                 :chars chars
                 :glyph-count glyph-count
                 :glyph-padding glyph-padding
                 :texture texture
                 :recs recs
                 :glyphs glyphs))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass default-font (font) ()))

(defmethod initialize-instance :around ((font default-font) &key)
  (c-let ((c-font claylib/ll:font))
    (claylib/ll:get-font-default c-font)
;    (let ((c-tex (claylib/ll:font.texture c-font))))
    (setf (c-struct font) c-font
          (path font) #p"default-font-has-no-path"
          (chars font) 0
          (size font) (claylib/ll:font.base-size c-font)
          (glyph-count font) (claylib/ll:font.glyph-count c-font)
          (glyph-padding font) (claylib/ll:font.glyph-padding c-font)
          #|
          (slot-value font '%texture) (make-instance 'rl-texture
          :id (claylib/ll:texture.id c-tex)
          :width (claylib/ll:texture.width c-tex)
          :height (claylib/ll:texture.height c-tex)
          :mipmaps (claylib/ll:texture.mipmaps c-tex)
          :data-format (claylib/ll:texture.format c-tex))
          (c-struct (texture font)) c-tex
          (texture font) (texture font)|#
          ))
  (trivial-garbage:finalize font
                            (let ((c (c-struct font)))
                              (lambda ()
                                (when (autowrap:valid-p c)
                                  (autowrap:free c))))))

(defparameter +default-font+ nil)

(defun load-font-default ()
  (make-instance 'default-font))
