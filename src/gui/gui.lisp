(in-package #:claylib)

;;; Mixin classes

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-object (linkable)
    ((%bounds :initarg :bounds
              :type rl-rectangle
              :accessor bounds)))

  (defclass text-label ()
    ((%text :initarg :text
            :type string
            :accessor text))
    (:default-initargs
     :text ""))

  (defclass text-box ()
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

  (defclass editable ()
    ((%edit-mode :initarg :edit-mode
                 :type boolean
                 :accessor edit-mode))
    (:default-initargs
     :edit-mode nil))

  (defclass int-values ()
    ((%value :initarg :value
             :type cffi:foreign-pointer)
     (%min-value :initarg :min-value
                 :type integer
                 :accessor min-value)
     (%max-value :initarg :max-value
                 :type integer
                 :accessor max-value)))

  (defclass value-bar ()
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

  (defclass gui-color ()
    ((%color :initarg :color
             :type rl-color
             :accessor color))))

(defmethod (setf min-value) :before (value (obj int-values))
  (set-linked-children 'min-value obj value))
(defmethod (setf max-value) :before (value (obj int-values))
  (set-linked-children 'max-value obj value))

(defmethod text ((text-box text-box))
  (plus-c:c-ref (slot-value text-box '%text) :char string))

(defmethod (setf text) ((value string) (text-box text-box))
  (let ((oldptr (slot-value text-box '%text)))
    (if (>= (length value) (text-size text-box))
        (let ((ptr (autowrap:alloc-string value)))
          (setf (slot-value text-box '%text) ptr
                (slot-value text-box '%text-size) (1+ (length value)))
          (autowrap:free oldptr))
        (cffi:lisp-string-to-foreign value
                                     oldptr
                                     (text-size text-box)))))

(defmethod (setf text-size) :before ((value integer) (text-box text-box))
  (set-linked-children 'text-size text-box value))

(defmethod (setf text-size) ((value integer) (text-box text-box))
  (when (/= value (text-size text-box))
    (let ((ptr (autowrap:calloc :char value))
          (old (slot-value text-box '%text)))
      (cffi:lisp-string-to-foreign (if (<= value (length (text text-box)))
                                       (subseq (text text-box) 0 (1- value))
                                       (text text-box))
                                   ptr
                                   value)
      (setf (slot-value text-box '%text-size) value
            (slot-value text-box '%text) ptr)
      (autowrap:free old))
    value))

(defmethod initialize-instance :after ((text-box text-box)
                                       &key text-size text &allow-other-keys)
  (let* ((len (max (1+ (length text))
                   text-size))
         (ptr (autowrap:calloc :char len)))
    (cffi:lisp-string-to-foreign text ptr len)
    (setf (slot-value text-box '%text) ptr
          (slot-value text-box '%text-size) len))
  text-box)

(defmethod value ((range int-values))
  (plus-c:c-ref (slot-value range '%value) :int))

(defmethod (setf value) (value (range int-values))
  (set-linked-children 'value range value)
  (setf (plus-c:c-ref (slot-value range '%value) :int) value))

(defmethod initialize-instance :after ((range int-values)
                                       &key value &allow-other-keys)
  (let ((ptr (autowrap:calloc :int)))
    (setf (plus-c:c-ref ptr :int) value
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
