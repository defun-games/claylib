(in-package #:claylib)

;;; Mixin classes

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-object ()
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
             :type integer  ; TODO: pointer
             :accessor value)
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

(defmethod text ((text-box text-box))
  (plus-c:c-ref (slot-value text-box '%text) :char string))

(defmethod (setf text) ((value string) (text-box text-box))
  (let ((oldptr (slot-value text-box '%text)))
    (if (or (not (cffi:pointerp oldptr))
            (>= (length value) (text-size text-box)))
        (let ((ptr (autowrap:alloc-string value)))
          (when (cffi:pointerp oldptr) (autowrap:free oldptr))
          (setf (slot-value text-box '%text) ptr))
        (cffi:lisp-string-to-foreign value
                                     oldptr
                                     (text-size text-box)))))

(defmethod (setf text-size) ((value integer) (text-box text-box))
  (when (/= value (text-size text-box))
    (let ((ptr (autowrap:calloc :char (1+ value))))
      (cffi:lisp-string-to-foreign (if (< value (length (text text-box)))
                                       (subseq (text text-box) 0 value)
                                       (text text-box))
                                   ptr
                                   (1+ value))
      (autowrap:free (slot-value text-box '%text))
      (setf (text text-box) ptr))
    value))

(defmethod initialize-instance :after ((text-box text-box)
                                       &key text-size text &allow-other-keys)
  (setf (text-size text-box) text-size
        (text text-box) text)
  text-box)



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
