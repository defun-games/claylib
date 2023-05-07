(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-texture (c-struct linkable)
    ()
    (:default-initargs
     :c-ptr (calloc 'claylib/ll:texture))))

(defcreader id rl-texture id texture)
(defcreader width rl-texture width texture)
(defcreader height rl-texture height texture)
(defcreader mipmaps rl-texture mipmaps texture)
(defcreader data-format rl-texture format texture)

(define-print-object rl-texture
    (id width height mipmaps data-format))

(defcwriter id rl-texture id texture integer)
(defcwriter width rl-texture width texture integer)
(defcwriter height rl-texture height texture integer)
(defcwriter mipmaps rl-texture mipmaps texture integer)
(defcwriter data-format rl-texture format texture integer)

(definitializer rl-texture
  :pt-accessors ((id integer)
                 (width integer)
                 (height integer)
                 (mipmaps integer)
                 (data-format integer))
  :unload (safe-unload-texture t))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass tex (linkable)
    ((%source :initarg :source
              :type rl-rectangle
              :accessor source)
     (%dest :initarg :dest
            :type rl-rectangle
            :accessor dest)
     (%origin :initarg :origin
              :type rl-vector2
              :accessor origin)
     (%rotation :initarg :rot
                :type number
                :reader rot)
     (%tint :initarg :tint
            :type rl-color
            :accessor tint))
    (:default-initargs
     :origin (make-vector2 0 0)
     :rot 0.0
     :tint +white+)))

(child-setter tex source dest origin tint)

(define-print-object tex
    (source dest origin rot tint))

(defwriter-float rot tex %rotation)

(definitializer tex
  :lisp-slots ((%source)
               (%dest)
               (%origin)
               (%rotation t)
               (%tint)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass texture (tex rl-texture)
    ((%filter :initarg :filter
              :type integer
              :reader filter)
     (%wrap :initarg :wrap
            :type integer
            :reader wrap))
    (:default-initargs
     :filter +texture-filter-point+
     :wrap +texture-wrap-repeat+)))

(child-setter texture filter wrap)

(define-print-object texture
    (filter wrap))

(defmethod (setf filter) ((value integer) (texture texture))
  (claylib/ll:set-texture-filter (c-ptr texture) value)
  (setf (slot-value texture '%filter) value))

(defmethod (setf wrap) ((value integer) (texture texture))
  (claylib/ll:set-texture-wrap (c-ptr texture) value)
  (setf (slot-value texture '%wrap) value))

(definitializer texture
  :lisp-slots ((%filter t)
               (%wrap t)))

(defmethod slot-unbound (_ (obj texture) (slot (eql '%source)))
  (setf (slot-value obj slot) (make-instance 'rl-rectangle
                                             :x 0 :y 0
                                             :width (width obj)
                                             :height (height obj))))

(defun make-empty-texture (&rest args &key filter wrap origin rot tint source dest &allow-other-keys)
  (declare (ignore filter wrap origin rot tint source dest))
  (apply #'make-instance 'texture
         args))

(defmethod draw-object ((obj texture))
  (claylib/ll:draw-texture-pro (c-ptr obj)
                               (c-ptr (source obj))
                               (c-ptr (dest obj))
                               (c-ptr (origin obj))
                               (rot obj)
                               (c-ptr (tint obj))))

(static-draw draw-texture-non-object texture)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass texture-object (tex)
    ((%asset :initarg :asset
             :type texture-asset
             :accessor asset))))

(defreader c-asset texture-object c-asset asset)

(defreader x texture-object x dest)
(defreader y texture-object y dest)
(defreader width texture-object width dest)
(defreader height texture-object height dest)

(define-print-object texture-object
    (asset c-asset x y width height))

(child-setter texture-object asset)

(defwriter x texture-object x dest)
(defwriter y texture-object y dest)
(defwriter width texture-object width dest)
(defwriter height texture-object height dest)

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

(definitializer texture-object
  :lisp-slots ((%asset)))

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
                               (c-ptr (source obj))
                               (c-ptr (dest obj))
                               (c-ptr (origin obj))
                               (rot obj)
                               (c-ptr (tint obj))))

(static-draw draw-texture-object texture-object)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-render-texture (c-struct linkable)
    ((%texture :initarg :texture
               :type texture
               :reader texture)
     (%depth :initarg :depth
             :type texture
             :reader depth))
    (:default-initargs
     :c-ptr (calloc 'claylib/ll:render-texture))))

(defcreader id rl-render-texture id render-texture)

(define-print-object rl-render-texture
    (id texture depth))

(defcwriter id rl-render-texture id render-texture integer)
(defcwriter-struct texture rl-render-texture texture render-texture texture
  id width height mipmaps data-format)
(defcwriter-struct depth rl-render-texture depth render-texture texture
  id width height mipmaps data-format)

(definitializer rl-render-texture
  :struct-slots ((%texture) (%depth))
  :pt-accessors ((id integer))
  :unload (safe-unload-render-texture t))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass render-texture (rl-render-texture) ()))

(define-print-object render-texture
    ())
