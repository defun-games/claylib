(in-package #:claylib)

(defclass rl-texture ()
  ((%c-struct
    :type claylib/ll:texture
    :initform (autowrap:alloc 'claylib/ll:texture)
    :accessor c-struct)))

(defcreader id rl-texture id texture)
(defcreader width rl-texture width texture)
(defcreader height rl-texture height texture)
(defcreader mipmaps rl-texture mipmaps texture)
(defcreader data-format rl-texture format texture)

(defcwriter id rl-texture id texture integer)
(defcwriter width rl-texture width texture integer)
(defcwriter height rl-texture height texture integer)
(defcwriter mipmaps rl-texture mipmaps texture integer)
(defcwriter data-format rl-texture format texture integer)

(definitializer rl-texture
    (id integer) (width integer) (height integer) (mipmaps integer) (data-format integer))

(default-free rl-texture)
(default-free-c claylib/ll:texture unload-texture t)



(defclass tex ()
  ((%filter :initarg :filter
            :type integer
            :reader filter)
   (%wrap :initarg :wrap
          :type integer
          :reader wrap)
   (%source :initarg :source
            :type rl-rectangle
            :accessor source)
   (%dest :initarg :dest
          :type rl-rectangle
          :accessor dest)
   (%origin :initarg :origin
            :type rl-vector2
            :accessor origin)
   (%rotation :initarg :rot
              :type (or float integer)
              :reader rot)
   (%tint :initarg :tint
          :type rl-color
          :accessor tint)))

(defwriter-float rot tex %rotation)

(defmethod (setf filter) ((value integer) (texture tex))
  (claylib/ll:set-texture-filter (c-struct texture) value)
  (setf (slot-value texture '%filter) value))

(defmethod (setf wrap) ((value integer) (texture tex))
  (claylib/ll:set-texture-wrap (c-struct texture) value)
  (setf (slot-value texture '%wrap) value))

(default-slot-value tex %origin (make-vector2 0 0))
(default-slot-value tex %rotation 0.0)
(default-slot-value tex %tint +white+)

(defmethod free ((obj tex))
  (when (slot-boundp obj '%source)
    (free (source obj)))
  (when (slot-boundp obj '%dest)
    (free (dest obj)))
  (free (origin obj))
  (when (next-method-p)
    (call-next-method)))



(defclass texture (rl-texture tex) ())

(defmethod slot-unbound (_ (obj texture) (slot (eql '%source)))
  (setf (slot-value obj slot) (make-instance 'rl-rectangle
                                             :x 0 :y 0
                                             :width (width obj)
                                             :height (height obj))))

(defun make-empty-texture (&rest args &key filter wrap origin rot tint source dest)
  (declare (ignore filter wrap origin rot tint source dest))
  (apply #'make-instance 'texture
         args))

(defmethod draw-object ((obj texture))
  (claylib/ll:draw-texture-pro (c-struct obj)
                               (c-struct (source obj))
                               (c-struct (dest obj))
                               (c-struct (origin obj))
                               (rot obj)
                               (c-struct (tint obj))))



(defclass texture-object (tex)
  ((%asset :initarg :asset
           :type texture-asset
           :accessor asset)))

(defreader c-asset texture-object c-asset asset)

(defmethod (setf texture) :before ((asset texture-asset) (obj texture-object))
  ;; Handle the case where a default source was set, and now the asset is changing.
  ;; We want to reset the source to match the new texture-asset.
  (when (and (slot-boundp obj '%source)
             (source obj)
             (= (x (source obj)) 0)
             (= (y (source obj)) 0)
             (= (width (source obj)) (width (asset obj)))
             (= (height (source obj)) (height (asset obj))))
    (setf (width (source obj)) (width asset)
          (height (source obj)) (height asset))))

(defmethod slot-unbound (_ (obj texture-object) (slot (eql '%source)))
  (setf (slot-value obj slot) (make-instance 'rl-rectangle
                                             :x 0 :y 0
                                             :width (width (asset obj))
                                             :height (height (asset obj)))))

(defun make-texture (texture-asset x y
                     &rest args &key width height filter wrap origin rot tint source)
  "Make a texture object ready for drawing. Loads TEXTURE-ASSET when not already loaded."
  (declare (ignore filter wrap origin rot tint source))
  (load-asset texture-asset)
  (apply #'make-instance 'texture-object
         :allow-other-keys t
         :asset texture-asset
         :dest (make-instance 'rl-rectangle
                              :x x
                              :y y
                              :width (or width (width texture-asset))
                              :height (or height (height texture-asset)))
         args))

(defun make-texture-from-rec (texture-asset dest
                              &rest args &key filter wrap origin rot tint source)
  (declare (ignore filter wrap origin rot tint source))
  (load-asset texture-asset)
  (apply #'make-instance 'texture-object
         :asset texture-asset
         :dest dest
         args))

(defmethod draw-object ((obj texture-object))
  (claylib/ll:draw-texture-pro (c-asset obj)
                               (c-struct (source obj))
                               (c-struct (dest obj))
                               (c-struct (origin obj))
                               (rot obj)
                               (c-struct (tint obj))))



(defclass rl-render-texture ()
  ((%texture :initarg :texture
             :type texture
             :reader texture)
   (%depth :initarg :depth
           :type texture
           :reader depth)
   (%c-struct
    :type claylib/ll:render-texture
    :initform (autowrap:alloc 'claylib/ll:render-texture)
    :accessor c-struct)))

(defcreader id rl-render-texture id render-texture)

(defcwriter id rl-render-texture id render-texture integer)
(defcwriter-struct texture rl-render-texture texture render-texture texture
  id width height mipmaps data-format)
(defcwriter-struct depth rl-render-texture depth render-texture texture
  id width height mipmaps data-format)

(definitializer rl-render-texture
    (id integer) (texture texture) (depth texture))

(default-free rl-render-texture)
(default-free-c claylib/ll:render-texture unload-render-texture)
