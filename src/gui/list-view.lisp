(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-list-view (gui-object text-label)
    ((%scroll-index :initarg :scroll-index
                    :type integer  ; TODO: pointer
                    :accessor scroll-index)
     (%active :initarg :active
              :type integer
              :accessor active)
     (%num :initarg :num
           :type integer
           :accessor num)
     (%focus :initarg :focus
             :type integer  ; TODO: pointer
             :accessor focus))
    (:default-initargs
     :scroll-index 0
     :active 0)
    (:documentation "List View control, sets ACTIVE to selected list item index")))

(defun gui-list-view (bounds text scroll-index active &key count focus)
  "List View control, returns selected list item index"
  (check-type bounds rl-rectangle)
  (check-type text string)
  (check-type scroll-index integer)
  (check-type active integer)
  (check-type count (or null integer))
  (check-type focus (or null integer))
  (if (and count focus)
      (claylib/ll:gui-list-view-ex (c-struct bounds) text count focus scroll-index active)
      (claylib/ll:gui-list-view (c-struct bounds) text scroll-index active)))

(defmethod draw-object ((obj gui-list-view))
  (setf (active obj)
        (gui-list-view (bounds obj)
                       (text obj)
                       (scroll-index obj)
                       (active obj)
                       :count (when (slot-boundp obj '%num) (num obj))
                       :focus (when (slot-boundp obj '%focus) (focus obj)))))

(defun make-gui-list-view (x y width height &rest args &key text scroll-index active num focus)
  (declare (ignorable text scroll-index active num focus))
  (apply #'make-instance 'gui-list-view
         :bounds (make-simple-rec x y width height)
         args))
