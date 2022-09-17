(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-color ()
    ((%c-struct
      :type claylib/ll:color
      :initform (autowrap:alloc 'claylib/ll:color)
      :reader c-struct))))

(defcreader r rl-color r color)
(defcreader g rl-color g color)
(defcreader b rl-color b color)
(defcreader a rl-color a color)

(defmethod free ((obj rl-color))
  (warn "BUG: Default RL-COLOR objects (~A) are not meant to be freed!" obj)
  obj)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass color (rl-color)
    ((%c-struct
      :accessor c-struct))))

(defcwriter r color r color integer)
(defcwriter g color g color integer)
(defcwriter b color b color integer)
(defcwriter a color a color integer)

(definitializer color
  :pt-accessors ((r integer)
                 (g integer)
                 (b integer)
                 (a integer)))

(defmethod free ((obj color))
  (when (and (c-struct obj)
             (autowrap:valid-p (c-struct obj)))
    (free (c-struct obj)))
  (setf (slot-value obj '%c-struct) nil)
  (trivial-garbage:cancel-finalization obj))

(default-free-c claylib/ll:color)

(defun make-color (r g b &optional (a 255))
  (make-instance 'color :r r :g g :b b :a a))

(defun copy-color (color)
  (if (typep color 'rl-color)
      (make-color (r color) (g color) (b color) (a color))
      (make-color (claylib/ll:color.r color)
                  (claylib/ll:color.g color)
                  (claylib/ll:color.b color)
                  (claylib/ll:color.a color))))

(defun copy-color-constant (color)
  (let ((ret (make-instance 'rl-color)))
    (setf (slot-value ret '%c-struct) color)
    ret))

;; TODO: This is required due to WITH-TEXTURE-MODE/CLEAR-BACKGROUND
;; but feels kind of hackish and I'd rather not need it.
(defmethod make-load-form ((obj rl-color) &optional environment)
  (declare (ignore environment))
  `(make-color ,(r obj) ,(g obj) ,(b obj) ,(a obj)))

(defvar +lightgray+ (copy-color-constant claylib/ll:+lightgray+))
(defvar +gray+ (copy-color-constant claylib/ll:+gray+))
(defvar +darkgray+ (copy-color-constant claylib/ll:+darkgray+))
(defvar +yellow+ (copy-color-constant claylib/ll:+yellow+))
(defvar +gold+ (copy-color-constant claylib/ll:+gold+))
(defvar +orange+ (copy-color-constant claylib/ll:+orange+))
(defvar +pink+ (copy-color-constant claylib/ll:+pink+))
(defvar +red+ (copy-color-constant claylib/ll:+red+))
(defvar +maroon+ (copy-color-constant claylib/ll:+maroon+))
(defvar +green+ (copy-color-constant claylib/ll:+green+))
(defvar +lime+ (copy-color-constant claylib/ll:+lime+))
(defvar +darkgreen+ (copy-color-constant claylib/ll:+darkgreen+))
(defvar +skyblue+ (copy-color-constant claylib/ll:+skyblue+))
(defvar +blue+ (copy-color-constant claylib/ll:+blue+))
(defvar +darkblue+ (copy-color-constant claylib/ll:+darkblue+))
(defvar +purple+ (copy-color-constant claylib/ll:+purple+))
(defvar +violet+ (copy-color-constant claylib/ll:+violet+))
(defvar +darkpurple+ (copy-color-constant claylib/ll:+darkpurple+))
(defvar +beige+ (copy-color-constant claylib/ll:+beige+))
(defvar +brown+ (copy-color-constant claylib/ll:+brown+))
(defvar +darkbrown+ (copy-color-constant claylib/ll:+darkbrown+))
(defvar +white+ (copy-color-constant claylib/ll:+white+))
(defvar +black+ (copy-color-constant claylib/ll:+black+))
(defvar +blank+ (copy-color-constant claylib/ll:+blank+))
(defvar +magenta+ (copy-color-constant claylib/ll:+magenta+))
(defvar +raywhite+ (copy-color-constant claylib/ll:+raywhite+))

(defvar *claylib-background* +raywhite+)
