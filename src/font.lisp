(in-package #:claylib)

(defclass rl-glyph-info ()
  ((%image :initarg :image
           :type rl-image
           :reader image)
   (%c-struct
    :type claylib/ll:glyph-info
    :initform (autowrap:alloc 'claylib/ll:glyph-info)
    :accessor c-struct)))

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
    (value integer) (offset-x integer) (offset-y integer) (advance-x integer) (image rl-image))

(default-free rl-glyph-info)
;; TODO: I think UNLOAD-FONT-DATA should be used here somehow.
(default-free-c claylib/ll:glyph-info)




(defclass rl-font ()
  ((%texture :initarg :texture
             :type rl-texture
             :reader texture)
   (%recs :initarg :recs
          ; :type TODO - pointer
          :reader recs)
   (%glyphs :initarg :glyphs
            ; :type TODO - pointer
            :reader glyphs)
   (%c-struct
    :type claylib/ll:font
    :initform (autowrap:alloc 'claylib/ll:font)
    :accessor c-struct)))

(defcreader size rl-font base-size font)
(defcreader glyph-count rl-font glyph-count font)
(defcreader glyph-padding rl-font glyph-padding font)

(defcwriter size rl-font base-size font integer)
(defcwriter glyph-count rl-font glyph-count font integer)
(defcwriter glyph-padding rl-font glyph-padding font integer)
(defcwriter-struct texture rl-font texture font texture
  id width height mipmaps data-format)
(defcwriter-struct recs rl-font recs font rectangle ; pointer
  x y width height)
(defcwriter-struct glyphs rl-font glyphs font glyph-info ; pointer
  value offset-x offset-y advance-x)

(definitializer rl-font
    (size integer) (glyph-count integer) (glyph-padding integer)
  (texture rl-texture) (recs t) (glyphs t))

(default-free rl-font)
(default-free-c claylib/ll:font unload-font)




(defclass font (rl-font game-asset)
  ((%chars :initarg :chars
           :type integer
           :accessor chars)))

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

(defclass default-font (font) ())

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
