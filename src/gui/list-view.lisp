(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-list-view (gui-object text-label)
    ((%scroll-index :initarg :scroll-index
                    :type cffi:foreign-pointer)
     (%active :initarg :active
              :type integer
              :accessor active)
     ;; TODO: num and focus don't currently do anything. We need an array of strings to
     ;; fully support GuiListViewEx.
     (%num :initarg :num
           :type integer)
     (%focus :initarg :focus
             :type cffi:foreign-pointer))
    (:default-initargs
     :scroll-index 0
     :active 0
     :focus -1)
    (:documentation "List View control, sets ACTIVE to selected list item index")))

(define-print-object gui-list-view
    (active))

(child-setter gui-list-view active scroll-index focus)

(defmethod scroll-index ((list-view gui-list-view))
  (cffi:mem-ref (slot-value list-view '%scroll-index) :int))

(defmethod focus ((list-view gui-list-view))
  (cffi:mem-ref (slot-value list-view '%focus) :int))

(defmethod (setf scroll-index) (value (list-view gui-list-view))
  (setf (cffi:mem-ref (slot-value list-view '%scroll-index) :int) value))

(defmethod (setf focus) (value (list-view gui-list-view))
  (setf (cffi:mem-ref (slot-value list-view '%focus) :int) value))

(defmethod initialize-instance :after ((list-view gui-list-view)
                                       &key scroll-index focus &allow-other-keys)
  (let ((ptr (calloc :int))
        (ptr2 (calloc :int)))
    (setf (cffi:mem-ref ptr :int) scroll-index
          (cffi:mem-ref ptr2 :int) focus
          (slot-value list-view '%scroll-index) ptr
          (slot-value list-view '%focus) ptr2)
    list-view))

(defun gui-list-view (bounds text scroll-index active &key count focus)
  "List View control, returns selected list item index"
  (check-type bounds rl-rectangle)
  (check-type text string)
  (check-type scroll-index cffi:foreign-pointer)
  (check-type active integer)
  (check-type count (or null integer))
  (check-type focus cffi:foreign-pointer)
  (if (and count focus)
      (claylib/ll:gui-list-view-ex (c-ptr bounds) text count focus scroll-index active)
      (claylib/ll:gui-list-view (c-ptr bounds) text scroll-index active)))

(defmethod draw-object ((obj gui-list-view))
  (setf (active obj)
        (gui-list-view (bounds obj)
                       (text obj)
                       (slot-value obj '%scroll-index)
                       (active obj)
                       :count (when (slot-boundp obj '%num) (num obj))
                       :focus (slot-value obj '%focus))))

(static-draw draw-gui-list-view-object gui-list-view)

(defun make-gui-list-view (x y width height &rest args &key text scroll-index active focus)
  (declare (ignorable text scroll-index active focus))
  (apply #'make-instance 'gui-list-view
         :bounds (make-simple-rec x y width height)
         args))
