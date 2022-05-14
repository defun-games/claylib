(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-color ()
    ((%c-struct
      :type claylib/ll:color
      :initform (autowrap:alloc 'claylib/ll:color)
      :accessor c-struct))))

(defcreader r rl-color r color)
(defcreader g rl-color g color)
(defcreader b rl-color b color)
(defcreader a rl-color a color)

(defcwriter r rl-color r color integer)
(defcwriter g rl-color g color integer)
(defcwriter b rl-color b color integer)
(defcwriter a rl-color a color integer)

(definitializer rl-color (r integer) (g integer) (b integer) (a integer))

(default-free rl-color)
(default-free-c claylib/ll:color)

(defun make-color (r g b &optional (a 255))
  (make-instance 'rl-color :r r :g g :b b :a a))

(defun-pt-arg0 fade claylib/ll:fade (make-color 0 0 0)
  "Destructively fade a color in/out to a specified alpha value, unless ALLOCATE-P is T,
in which case create a new COLOR object as the return value."
  (color rl-color)
  (alpha number float))

(defun copy-color (color)
  (if (typep color 'rl-color)
      (make-color (r color) (g color) (b color) (a color))
      (make-color (claylib/ll:color.r color)
                  (claylib/ll:color.g color)
                  (claylib/ll:color.b color)
                  (claylib/ll:color.a color))))

(defvar +lightgray+ (copy-color claylib/ll:+lightgray+))
(defvar +gray+ (copy-color claylib/ll:+gray+))
(defvar +darkgray+ (copy-color claylib/ll:+darkgray+))
(defvar +yellow+ (copy-color claylib/ll:+yellow+))
(defvar +gold+ (copy-color claylib/ll:+gold+))
(defvar +orange+ (copy-color claylib/ll:+orange+))
(defvar +pink+ (copy-color claylib/ll:+pink+))
(defvar +red+ (copy-color claylib/ll:+red+))
(defvar +maroon+ (copy-color claylib/ll:+maroon+))
(defvar +green+ (copy-color claylib/ll:+green+))
(defvar +lime+ (copy-color claylib/ll:+lime+))
(defvar +darkgreen+ (copy-color claylib/ll:+darkgreen+))
(defvar +skyblue+ (copy-color claylib/ll:+skyblue+))
(defvar +blue+ (copy-color claylib/ll:+blue+))
(defvar +darkblue+ (copy-color claylib/ll:+darkblue+))
(defvar +purple+ (copy-color claylib/ll:+purple+))
(defvar +violet+ (copy-color claylib/ll:+violet+))
(defvar +darkpurple+ (copy-color claylib/ll:+darkpurple+))
(defvar +beige+ (copy-color claylib/ll:+beige+))
(defvar +brown+ (copy-color claylib/ll:+brown+))
(defvar +darkbrown+ (copy-color claylib/ll:+darkbrown+))
(defvar +white+ (copy-color claylib/ll:+white+))
(defvar +black+ (copy-color claylib/ll:+black+))
(defvar +blank+ (copy-color claylib/ll:+blank+))
(defvar +magenta+ (copy-color claylib/ll:+magenta+))
(defvar +raywhite+ (copy-color claylib/ll:+raywhite+))

(defvar *claylib-background* +raywhite+)
