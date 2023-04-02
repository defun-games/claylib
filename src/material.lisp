(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-shader (c-struct linkable)
    ()
    (:default-initargs
     :c-ptr (calloc 'claylib/ll:shader))))

(defcreader id rl-shader id shader)

(define-print-object rl-shader
    (id))

(defmethod loc ((shader rl-shader) (index integer))
  (when (and (< index 32) (>= index 0))
    (cffi:mem-aref (cffi:mem-ref (field-ptr (c-ptr shader) 'shader 'locs) :pointer) :int index)))
(defmethod locs ((shader rl-shader))
  (loop for i below 32
        collect (loc shader i)))

(defcwriter id rl-shader id shader integer)
(defmethod (setf loc) ((value integer) (shader rl-shader) (index integer))
  (when (and (< index 32) (>= index 0))
    (setf (cffi:mem-aref (cffi:mem-ref (field-ptr (c-ptr shader) 'shader 'locs) :pointer)
                         :int
                         index)
          value)))
(defmethod (setf locs) ((value sequence) (shader rl-shader))
  (dotimes (i 32)
    (setf (loc shader i) (if (< i (length value))
                             (elt value i)
                             0))))

(definitializer rl-shader
  :pt-accessors ((id integer)
                 (locs sequence))
  :unload (safe-unload-shader t))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass shader (rl-shader) ()))

(define-print-object shader
    ())


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-material-map (c-struct linkable)
    ((%texture :initarg :texture
               :type rl-texture
               :reader texture)
     (%color :initarg :color
             :type rl-color
             :reader color))
    (:default-initargs
     :c-ptr (calloc 'claylib/ll:material-map))))

(defcreader value rl-material-map value material-map)

(define-print-object rl-material-map
    (texture color value))

(defcwriter value rl-material-map value material-map number float)
(defcwriter-struct texture rl-material-map texture material-map texture
  id width height mipmaps data-format)
(defcwriter-struct color rl-material-map color material-map color
  r g b a)

(definitializer rl-material-map
  :struct-slots ((%texture)
                 (%color color))
  :pt-accessors ((value number float)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass material-map (rl-material-map) ()))

(define-print-object material-map
    ())


(defconstant +foreign-material-map-size+ (cffi:foreign-type-size 'claylib/ll:material-map))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-material-maps (rl-sequence)
    ((%cl-array :type (array rl-material-map 1)))))

(define-print-object rl-material-maps
    ())

(defun make-rl-material-map-array (c-ptr num &optional finalize)
  (let ((contents (loop for i below num
                        for c-elt = (cffi:mem-aref c-ptr 'claylib/ll:material-map i)
                        for map = (make-instance 'rl-material-map
                                                 :c-ptr c-elt
                                                 :finalize (when finalize (= i 0))
                                                 :texture (make-instance
                                                           'rl-texture
                                                           :finalize nil
                                                           :c-ptr (field-value c-elt
                                                                               'material-map
                                                                               'texture))
                                                 :color (make-instance
                                                         'color
                                                         :finalize nil
                                                         :c-ptr (field-value c-elt
                                                                             'material-map
                                                                             'color)))
                        collect map)))
    (make-array num
                :element-type 'rl-material-map
                :initial-contents contents)))

(defmethod (setf sequences:elt) (value (sequence rl-material-maps) index)
  (check-type value rl-material-map)
  (cffi:foreign-funcall "memcpy"
                        :pointer (c-ptr (elt sequence index))
                        :pointer (c-ptr value)
                        :int +foreign-material-map-size+
                        :void))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-material (c-struct linkable)
    ((%shader :initarg :shader
              :type rl-shader
              :reader shader)
     (%maps :initarg :maps
            :type rl-material-maps
            :reader maps))
    (:default-initargs
     :c-ptr (calloc 'claylib/ll:material))))

(define-print-object rl-material
    (shader maps))

(defmethod param ((material rl-material) (index integer))
  (when (and (< index 4) (>= index 0))
    (cffi:mem-aref (field-ptr (c-ptr material) 'material 'params)
                   :float
                   index)))
(defmethod params ((material rl-material))
  (loop for i below 4
        collect (param material i)))

(defcwriter-struct shader rl-material shader material shader
  id (shader locs))
(defcwriter-struct maps rl-material maps material material-map ; Array/pointer
  texture color value)
(defmethod (setf matmap) ((value rl-material-map) (material rl-material) (index integer))
  (when (and (< index 11) (>= index 0))
    (cffi:foreign-funcall "memcpy"
                          :pointer (cffi:mem-aptr (field-ptr (c-ptr material) 'material 'maps)
                                                  'claylib/ll:material-map
                                                  index)
                          :pointer (c-ptr value)
                          :int +foreign-material-map-size+
                          :void)))
(defmethod (setf maps) ((value rl-material-maps) (material rl-material))
  (when (cffi-sys:null-pointer-p (field-ptr (c-ptr material) 'material 'maps))
    (setf (field-value (c-ptr material) 'material 'maps)
          (calloc 'claylib/ll:material-map (length value))))
  (dotimes (i 11)
    (setf (matmap material i) (elt value i))))
(defmethod (setf param) ((value number) (material rl-material) (index integer))
  (when (and (< index 4) (>= index 0))
    (setf (cffi:mem-aref (field-ptr (c-ptr material) 'material 'params)
                         :float
                         index)
          (coerce value 'float))))
(defmethod (setf params) ((value sequence) (material rl-material))
  (dotimes (i 4)
    (setf (param material i) (if (< i (length value))
                                 (elt value i)
                                 0))))

(definitializer rl-material
  :lisp-slots ((%maps))
  :struct-slots ((%shader))
  :pt-accessors ((params sequence))
  :unload (safe-unload-material t))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass material (rl-material) ()))

(define-print-object material
    ())


(defconstant +foreign-material-size+ (cffi:foreign-type-size 'claylib/ll:material))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-materials (rl-sequence)
    ((%cl-array :type (array rl-material 1)))))

(define-print-object rl-materials
    ())

(defun make-rl-material-array (c-ptr num &optional finalize)
  (let ((contents
          (loop
            for i below num
            for c-elt = (cffi:mem-aref c-ptr 'claylib/ll:material i)
            for mat = (make-instance 'rl-material
                                     :finalize (when finalize (= i 0))
                                     :c-ptr c-elt
                                     :shader (make-instance 'rl-shader
                                                            :finalize nil
                                                            :c-ptr (field-value c-elt
                                                                                'material
                                                                                'shader)))
            do (setf (slot-value mat '%maps)
                     (let ((maps (make-instance 'rl-material-maps)))
                       (setf (slot-value maps '%cl-array)
                             (make-rl-material-map-array
                              (cffi:mem-ref (field-ptr c-elt 'material 'maps) :pointer)
                              11)) ; TODO 11 entries in MaterialMapIndex, is this the correct num?
                       maps))
            collect mat)))
    (make-array num
                :element-type 'rl-material
                :initial-contents contents)))

(defmethod (setf sequences:elt) (value (sequence rl-materials) index)
  (check-type value rl-material)
  (cffi:foreign-funcall "memcpy"
                        :pointer (c-ptr (elt sequence index))
                        :pointer (c-ptr value)
                        :int +foreign-material-size+
                        :void))
