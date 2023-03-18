(in-package #:claylib)

;;; Mixin classes

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-object (linkable)
    ((%bounds :initarg :bounds
              :type rl-rectangle
              :accessor bounds)))

  (defclass text-label (linkable)
    ((%text :initarg :text
            :type string
            :accessor text))
    (:default-initargs
     :text ""))

  (defclass text-box (linkable)
    ((%text-size :initarg :text-size
                 :type integer
                 :reader text-size)
     (%text :initarg :text
            :type cffi:foreign-pointer))
    (:default-initargs
     :text-size 0
     :text ""))

  (defclass pressable ()
    ((%pressed
      :initform nil
      :type boolean
      :reader pressed
      :documentation "The 'pressed' slot is a return value of the drawing function. It rarely makes
sense to set this in your own code.")))

  (defclass editable (linkable)
    ((%edit-mode :initarg :edit-mode
                 :type boolean
                 :accessor edit-mode))
    (:default-initargs
     :edit-mode nil))

  (defclass int-values (linkable)
    ((%value :initarg :value
             :type cffi:foreign-pointer)
     (%min-value :initarg :min-value
                 :type integer
                 :accessor min-value)
     (%max-value :initarg :max-value
                 :type integer
                 :accessor max-value)))

  (defclass value-bar (linkable)
    ((%text-left :initarg :text-left
                 :type string
                 :accessor text-left)
     (%text-right :initarg :text-right
                  :type string
                  :accessor text-right)
     (%value :initarg :value
             :type number
             :reader value)
     (%min-value :initarg :min-value
                 :type number
                 :reader min-value)
     (%max-value :initarg :max-value
                 :type number
                 :reader max-value))
    (:default-initargs
     :text-left ""
     :text-right ""))
  (defwriter-float value value-bar)
  (defwriter-float min-value value-bar)
  (defwriter-float max-value value-bar)

  (defclass gui-color (linkable)
    ((%color :initarg :color
             :type rl-color
             :accessor color))))

(child-setter gui-object bounds)
(child-setter text-label text)
(child-setter text-box text-size text)
(child-setter editable edit-mode)
(child-setter int-values value min-value max-value)
(child-setter value-bar text-left text-right)
(child-setter gui-color color)

(define-print-object gui-object
    (bounds))

(define-print-object text-label
    (text))

(define-print-object text-box
    (text-size))

(define-print-object pressable
    (pressed))

(define-print-object editable
    (edit-mode))

(define-print-object int-values
    (min-value max-value))

(define-print-object value-bar
    (text-left text-right value min-value max-value))

(define-print-object gui-color
    (color))

(defmethod text ((text-box text-box))
  (cffi:foreign-string-to-lisp (slot-value text-box '%text)))

(defmethod (setf text) ((value string) (text-box text-box))
  (let ((oldptr (slot-value text-box '%text)))
    (if (>= (length value) (text-size text-box))
        (let ((ptr (cffi:foreign-string-alloc value)))
          (setf (slot-value text-box '%text) ptr
                (slot-value text-box '%text-size) (1+ (length value)))
          (cffi:foreign-free oldptr))
        (cffi:lisp-string-to-foreign value
                                     oldptr
                                     (text-size text-box)))))

(child-setter text-box text-size)

(defmethod (setf text-size) ((value integer) (text-box text-box))
  (when (/= value (text-size text-box))
    (let ((ptr (calloc :char value))
          (old (slot-value text-box '%text)))
      (cffi:lisp-string-to-foreign (if (<= value (length (text text-box)))
                                       (subseq (text text-box) 0 (1- value))
                                       (text text-box))
                                   ptr
                                   value)
      (setf (slot-value text-box '%text-size) value
            (slot-value text-box '%text) ptr)
      (cffi:foreign-free old))
    value))

(defmethod initialize-instance :after ((text-box text-box)
                                       &key text-size text &allow-other-keys)
  (let* ((len (max (1+ (length text))
                   text-size))
         (ptr (calloc :char len)))
    (cffi:lisp-string-to-foreign text ptr len)
    (setf (slot-value text-box '%text) ptr
          (slot-value text-box '%text-size) len))
  text-box)

(defmethod value ((range int-values))
  (cffi:mem-ref (slot-value range '%value) :int))

(defmethod (setf value) (value (range int-values))
  (setf (cffi:mem-ref (slot-value range '%value) :int) value))

(defmethod initialize-instance :after ((range int-values)
                                       &key value &allow-other-keys)
  (let ((ptr (calloc :int)))
    (setf (cffi:mem-ref ptr :int) value
          (slot-value range '%value) ptr)
    range))



;;; Font set/get functions

(defun-pt-void gui-set-font claylib/ll:gui-set-font
  "Set gui custom font (global state)"
  (font rl-font))

(defun-pt gui-get-font claylib/ll:gui-get-font
  "Get gui custom font (global state). Allocates a new RL-FONT unless you pass one."
  (font rl-font nil (make-instance 'rl-font)))



;;; Icons functionality

(defun-pt-bool gui-check-icon-pixel claylib/ll:gui-check-icon-pixel
  "Check icon pixel value"
  (icon-id integer)
  (x integer)
  (y integer))
