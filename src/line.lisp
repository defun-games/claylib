(in-package #:claylib)

(defclass line ()
  ((%start :initarg :start
           :type vec
           :accessor start)
   (%end :initarg :end
         :type vec
         :accessor end)
   (%color :initarg :color
           :type rl-color
           :accessor color)))

(defreader x1 line x start)
(defreader y1 line y start)
(defreader x2 line x end)
(defreader y2 line y end)

(defwriter x1 line x start number)
(defwriter y1 line y start number)
(defwriter x2 line x end number)
(defwriter y2 line y end number)

(definitializer line
    (start vec nil) (end vec nil) (color rl-color nil))

(defmethod free ((obj line))
  (mapcar #'free (list (start obj)
                       (end obj)))
  (when (next-method-p)
    (call-next-method)))



(defclass line-2d (line)
  ((%start :type rl-vector2)
   (%end :type rl-vector2)
   (%thickness :initarg :thickness
               :type (or integer float)
               :reader thickness)))

(defwriter-float thickness line-2d)

(definitializer-float line-2d thickness)

(default-slot-value line-2d %thickness 1.0)

(defun make-line-2d (x1 y1 x2 y2 color &rest args &key thickness)
  (declare (ignore thickness))
  (apply #'make-instance 'line-2d
         :start (make-vector2 x1 y1)
         :end (make-vector2 x2 y2)
         :color color
         args))

(defun make-line-2d-from-vecs (start end color &rest args &key thickness)
  (declare (ignore thickness))
  (apply #'make-instance 'line-2d
         :start start
         :end end
         :color color
         args))

(defmethod draw-object ((obj line-2d))
  (claylib/ll:draw-line-ex (c-struct (start obj))
                           (c-struct (end obj))
                           (thickness obj)
                           (c-struct (color obj))))
