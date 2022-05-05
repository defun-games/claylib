(in-package #:claylib)

(defclass cube (3d-shape)
  ((%size :initarg :size
          :type rl-vector3
          :accessor size)
   (%texture :initarg :texture
             :type rl-texture
             :accessor texture)
   (%source :initarg :source
            :type rl-rectangle
            :accessor source)))

(defreader width cube x size)
(defreader height cube y size)
(defreader len cube z size)

(defwriter width cube x size number)
(defwriter height cube y size number)
(defwriter len cube z size number)

(defmethod (setf texture) :before ((tex rl-texture) (obj cube))
  (when (and (slot-boundp obj '%source)
             (source obj)
             (= (x (source obj)) 0)
             (= (y (source obj)) 0)
             (= (width (source obj)) (width (texture obj)))
             (= (height (source obj)) (height (texture obj))))
    (setf (width (source obj)) (width tex)
          (height (source obj)) (height tex))))

(definitializer cube
    (size rl-vector3 nil) (texture rl-texture nil) (source rl-rectangle nil))

(defmethod free ((obj cube))
  (when (slot-boundp obj '%texture)
    (free (texture obj)))
  (when (slot-boundp obj '%source)
    (free (source obj)))
  (free (size obj))
  (setf (size obj) nil
        (texture obj) nil
        (source obj) nil)
  (when (next-method-p)
    (call-next-method)))

(defmethod slot-unbound (cube obj (slot (eql '%source)))
  (setf (slot-value obj slot) (make-instance 'rl-rectangle
                                             :x 0 :y 0
                                             :width (width (texture obj))
                                             :height (height (texture obj)))))

(defun make-cube (x y z width height length color &rest args &key filled texture source)
  (declare (ignore filled texture source))
  (apply #'make-instance 'cube
         :pos (make-vector3 x y z)
         :size (make-vector3 width height length)
         :color color
         args))

(defun make-cube-from-vecs (pos size color &rest args &key filled texture source)
  (declare (ignore filled texture source))
  (apply #'make-instance 'cube
         :pos pos
         :size size
         :color color
         args))

(defmethod draw-object ((obj cube))
  (cond
    ((and (slot-boundp obj '%texture) (texture obj) (filled obj))
     (claylib/ll:draw-cube-texture-rec (c-struct (texture obj))
                                       (c-struct (source obj))
                                       (c-struct (pos obj))
                                       (width obj)
                                       (height obj)
                                       (len obj)
                                       (c-struct (color obj))))
    ((filled obj)
     (claylib/ll:draw-cube-v (c-struct (pos obj))
                             (c-struct (size obj))
                             (c-struct (color obj))))
    (t (claylib/ll:draw-cube-wires-v (c-struct (pos obj))
                                     (c-struct (size obj))
                                     (c-struct (color obj))))))
