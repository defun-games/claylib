(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass line (linkable)
    ((%start :initarg :start
             :type vec
             :accessor start)
     (%end :initarg :end
           :type vec
           :accessor end)
     (%color :initarg :color
             :type rl-color
             :accessor color))))

(defreader x1 line x start)
(defreader y1 line y start)
(defreader x2 line x end)
(defreader y2 line y end)

(defwriter x1 line x start number)
(defwriter y1 line y start number)
(defwriter x2 line x end number)
(defwriter y2 line y end number)

(definitializer line
  :lisp-slots ((%start) (%end) (%color)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass line-2d (line)
    ((%start :type rl-vector2)
     (%end :type rl-vector2)
     (%thickness :initarg :thickness
                 :type number
                 :reader thickness)
     (%bezier :initarg :bezier
              :type boolean
              :accessor bezier)
     (%control-pt :initarg :control-pt
                  :type rl-vector2
                  :accessor control-pt)
     (%control-pt2 :initarg :control-pt2
                   :type rl-vector2
                   :accessor control-pt2))
    (:default-initargs
     :thickness 1.0
     :bezier nil)))

(defwriter-float thickness line-2d)

(definitializer line-2d
  :lisp-slots ((%thickness t)
               (%bezier)
               (%control-pt)
               (%control-pt2)))

(defun make-line-2d (x1 y1 x2 y2 color
                     &rest args &key thickness bezier control-pt control-pt2)
  (declare (ignore thickness bezier control-pt control-pt2))
  (apply #'make-instance 'line-2d
         :start (make-vector2 x1 y1)
         :end (make-vector2 x2 y2)
         :color color
         args))

(defun make-line-2d-from-vecs (start end color
                               &rest args &key thickness bezier control-pt control-pt2)
  (declare (ignore thickness bezier control-pt control-pt2))
  (apply #'make-instance 'line-2d
         :start start
         :end end
         :color color
         args))

(defmethod draw-object ((obj line-2d))
  (with-accessors ((bezier bezier) (thick thickness)) obj
    (let ((method (cond ((and (slot-boundp obj '%control-pt2)
                              (slot-boundp obj '%control-pt)
                              bezier)
                         0)
                        ((and (slot-boundp obj '%control-pt)
                              bezier)
                         1)
                        (bezier 2)
                        (t 3)))
          (start (c-struct (start obj)))
          (end (c-struct (end obj)))
          (color (c-struct (color obj))))
      (case method
        (0 (claylib/ll:draw-line-bezier-cubic start end
                                              (c-struct (control-pt obj))
                                              (c-struct (control-pt2 obj))
                                              thick color))
        (1 (claylib/ll:draw-line-bezier-quad start end
                                             (c-struct (control-pt obj))
                                             thick color))
        (2 (claylib/ll:draw-line-bezier start end thick color))
        (3 (claylib/ll:draw-line-ex start end thick color))))))
