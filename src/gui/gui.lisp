(in-package #:claylib)

;;;; Raygui Lispification
;;;; Obligatory disclaimer: Everything in this file is in flux and will probably get moved.
;;;; I don't fully know what direction this will go yet.



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
                 :accessor text-size)))

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
