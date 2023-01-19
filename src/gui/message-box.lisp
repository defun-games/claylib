(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-message-box (gui-object)
    ((%title :initarg :title
             :type string
             :accessor title)
     (%message :initarg :message
               :type string
               :accessor message)
     (%buttons :initarg :buttons
               :type string
               :accessor buttons)
     (%selected
      :initform -1
      :type integer
      :reader selected
      :documentation "The 'selected' slot is a return value of the drawing function. It rarely makes
sense to set this in your own code."))
    (:default-initargs
     :title ""
     :message ""
     :buttons "")
    (:documentation "Message Box control, displays a message, sets SELECTED to clicked button from list")))

(define-print-object gui-message-box
    (title message buttons selected))

(defun-pt-num gui-message-box claylib/ll:gui-message-box
  "Message Box control, displays a message"
  (bounds rl-rectangle)
  (title string)
  (message string)
  (buttons string))

(defmethod draw-object ((obj gui-message-box))
  (setf (slot-value obj '%selected)
        (gui-message-box (bounds obj)
                         (title obj)
                         (message obj)
                         (buttons obj))))

(defun make-gui-message-box (x y width height &rest args &key title message buttons)
  (declare (ignorable title message buttons))
  (apply #'make-instance 'gui-message-box
         :bounds (make-simple-rec x y width height)
         args))
