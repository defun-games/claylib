(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gui-scroll-panel (gui-object text-label)
    ((%content :initarg :content
               :type rl-rectangle
               :accessor content)
     (%scroll :initarg :scroll
              :type rl-vector2
              :accessor scroll)
     (%view
      :initform (make-simple-rec 0 0 1 1)
      :type rl-rectangle
      :reader view
      :documentation "The GUI-SCROLL-PANEL view rectangle is a libffi return value only. It rarely makes sense to
set this in your own code."))
    (:default-initargs
     :scroll (make-vector2 0 0))
    (:documentation "Scroll Panel control")))

(define-print-object gui-scroll-panel
  (content scroll view))

(defun-pt gui-scroll-panel claylib/ll:gui-scroll-panel
  "Scroll Panel control. Allocates a new RL-RECTANGLE unless you pass one."
  (rec rl-rectangle nil (make-instance 'rl-rectangle))
  (bounds rl-rectangle)
  (text string)
  (content rl-rectangle)
  (scroll autowrap:wrapper))

(defmethod draw-object ((obj gui-scroll-panel))
  (gui-scroll-panel (bounds obj)
                    (text obj)
                    (content obj)
                    (c-struct (scroll obj))
                    :rec (view obj)))

(defun make-gui-scroll-panel (bounds-x bounds-y bounds-width bounds-height
                              content-x content-y content-width content-height
                              &rest args &key text scroll)
  (declare (ignorable text scroll))
  (apply #'make-instance 'gui-scroll-panel
         :bounds (make-simple-rec bounds-x bounds-y bounds-width bounds-height)
         :content (make-simple-rec content-x content-y content-width content-height)
         args))

(defun make-gui-scroll-panel-from-recs (bounds content &rest args &key text scroll)
  (declare (ignorable text scroll))
  (apply #'make-instance 'gui-scroll-panel
         :bounds bounds
         :content content
         args))
