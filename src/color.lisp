(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-color (c-struct linkable)
    ()
    (:default-initargs
     :c-ptr (calloc 'claylib/ll:color))))

(defcreader r rl-color r color)
(defcreader g rl-color g color)
(defcreader b rl-color b color)
(defcreader a rl-color a color)

(define-print-object rl-color
    (r g b a))

(definitializer rl-color)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass color (rl-color) ()))

(define-print-object color
    ())

(defcwriter r color r color integer)
(defcwriter g color g color integer)
(defcwriter b color b color integer)
(defcwriter a color a color integer)

(definitializer color
  :pt-accessors ((r integer)
                 (g integer)
                 (b integer)
                 (a integer)))

(defun make-color (r g b &optional (a 255))
  (make-instance 'color :r r :g g :b b :a a))

(defun copy-color (color &optional into)
  "Copy a given COLOR by allocating a new one unless an existing rl-color, INTO, is given."
  (if into
      ;; TODO handle case where color is not an rl-color (∴ cannot use rgba accessors)
      (setf (r into) (r color)
            (g into) (g color)
            (b into) (b color)
            (a into) (a color))
      (if (typep color 'rl-color)
          (make-color (r color) (g color) (b color) (a color))
          (make-color (field-value color 'color 'r)
                      (field-value color 'color 'g)
                      (field-value color 'color 'b)
                      (field-value color 'color 'a)))))

(defun copy-color-constant (color)
  (make-instance 'rl-color :c-ptr color))

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
