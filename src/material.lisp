(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-shader ()
    ((%c-struct
      :type claylib/ll:shader
      :accessor c-struct))
    (:default-initargs
     :c-struct (autowrap:calloc 'claylib/ll:shader))))

(defcreader id rl-shader id shader)
(defmethod loc ((shader rl-shader) (index integer))
  (when (and (< index 32) (>= index 0))
    (autowrap:c-aref (shader.locs (c-struct shader)) index :int)))
(defmethod locs ((shader rl-shader))
  (loop for i below 32
        collect (loc shader i)))

(defcwriter id rl-shader id shader integer)
(defmethod (setf loc) ((value integer) (shader rl-shader) (index integer))
  (when (and (< index 32) (>= index 0))
    (setf (autowrap:c-aref (shader.locs (c-struct shader)) index :int) value)))
(defmethod (setf locs) ((value sequence) (shader rl-shader))
  (dotimes (i 32)
    (setf (loc shader i) (if (< i (length value))
                             (elt value i)
                             0))))

(definitializer rl-shader
  :pt-accessors ((id integer)
                 (locs sequence)))

(default-free rl-shader)
(default-free-c claylib/ll:shader unload-shader)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass shader (rl-shader) ()))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-material-map ()
    ((%texture :initarg :texture
               :type rl-texture
               :reader texture)
     (%color :initarg :color
             :type rl-color
             :reader color)
     (%c-struct
      :type claylib/ll:material-map
      :accessor c-struct))
    (:default-initargs
     :c-struct (autowrap:calloc 'claylib/ll:material-map))))

(defcreader value rl-material-map value material-map)

(defcwriter value rl-material-map value material-map number float)
(defcwriter-struct texture rl-material-map texture material-map texture
  id width height mipmaps data-format)
(defcwriter-struct color rl-material-map color material-map color
  r g b a)

(definitializer rl-material-map
  :struct-slots ((%texture)
                 (%color color))
  :pt-accessors ((value number float)))

(default-free rl-material-map %texture %color)
(default-free-c claylib/ll:material-map)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass material-map (rl-material-map) ()))



(cffi:defcstruct texture-2d
  (id :uint)
  (width :int)
  (height :int)
  (mipmaps :int)
  (format :int))
(cffi:defcstruct color
  (r :unsigned-char)
  (g :unsigned-char)
  (b :unsigned-char)
  (a :unsigned-char))
(cffi:defcstruct material-map
  (texture (:struct texture-2d))
  (color (:struct color))
  (value :float))
(defconstant +foreign-material-map-size+ (cffi:foreign-type-size '(:struct material-map)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-material-maps (rl-sequence)
    ((%cl-array :type (array rl-material-map 1)))))

(defmethod make-rl-*-array ((c-struct claylib/wrap:material-map) num)
  (let ((contents (loop for i below num
                        for map = (make-instance 'rl-material-map)
                        for c-elt = (autowrap:c-aref c-struct i 'claylib/wrap:material-map)
                        do (setf (slot-value map '%c-struct)
                                 c-elt

                                 (slot-value map '%texture)
                                 (let ((tex (make-instance 'rl-texture)))
                                   (setf (c-struct tex) (material-map.texture c-elt))
                                   tex)

                                 (slot-value map '%color)
                                 (let ((col (make-instance 'color)))
                                   (setf (c-struct col) (material-map.color c-elt))
                                   col))
                        collect map)))
    (make-array num
                :element-type 'rl-material-map
                :initial-contents contents)))

(defmethod (setf sequences:elt) (value (sequence rl-material-maps) index)
  (check-type value rl-material-map)
  (cffi:foreign-funcall "memcpy"
                        :pointer (autowrap:ptr (c-struct (elt sequence index)))
                        :pointer (autowrap:ptr (c-struct value))
                        :int +foreign-material-map-size+
                        :void))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-material ()
    ((%shader :initarg :shader
              :type rl-shader
              :reader shader)
     (%maps :initarg :maps
            :type rl-material-maps
            :reader maps)
     (%c-struct
      :type claylib/ll:material
      :accessor c-struct))
    (:default-initargs
     :c-struct (autowrap:calloc 'claylib/ll:material))))

(defmethod param ((material rl-material) (index integer))
  (when (and (< index 4) (>= index 0))
    (material.params[] (c-struct material) index)))
(defmethod params ((material rl-material))
  (loop for i below 4
        collect (param material i)))

(defcwriter-struct shader rl-material shader material shader
  id locs)
(defcwriter-struct maps rl-material maps material material-map ; Array/pointer
  texture color value)
(defmethod (setf matmap) ((value rl-material-map) (material rl-material) (index integer))
  (when (and (< index 11) (>= index 0))
    (cffi:foreign-funcall "memcpy"
                          :pointer (autowrap:c-aptr (material.maps (c-struct material))
                                                    index
                                                    'claylib/ll:material-map)
                          :pointer (autowrap:ptr (c-struct value))
                          :int +foreign-material-map-size+
                          :void)))
(defmethod (setf maps) ((value rl-material-maps) (material rl-material))
  (when (cffi-sys:null-pointer-p (material.maps (c-struct material)))
    (setf (material.maps (c-struct material))
          (autowrap:ptr (autowrap:calloc 'claylib/ll:material-map (length value)))))
  (dotimes (i 11)
    (setf (matmap material i) (elt value i))))
(defmethod (setf param) ((value number) (material rl-material) (index integer))
  (when (and (< index 4) (>= index 0))
    (setf (material.params[] (c-struct material) index) (coerce value 'float))))
(defmethod (setf params) ((value sequence) (material rl-material))
  (dotimes (i 4)
    (setf (param material i) (if (< i (length value))
                                 (elt value i)
                                 0))))

(definitializer rl-material
  :lisp-slots ((%maps))
  :struct-slots ((%shader))
  :pt-accessors ((params sequence)))

(default-free rl-material %shader %maps)
(default-free-c claylib/ll:material unload-material)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass material (rl-material) ()))



(cffi:defcstruct shader
  (id :uint)
  (locs :pointer))
(cffi:defcstruct material
  (shader (:struct shader))
  (maps :pointer)
  (params :float :count 4))
(defconstant +foreign-material-size+ (cffi:foreign-type-size '(:struct material)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-materials (rl-sequence)
    ((%cl-array :type (array rl-material 1)))))

(defmethod make-rl-*-array ((c-struct claylib/wrap:material) num)
  (let ((contents
          (loop
            for i below num
            for mat = (make-instance 'rl-material)
            for c-elt = (autowrap:c-aref c-struct i 'claylib/wrap:material)
            do (setf (slot-value mat '%c-struct)
                     c-elt

                     (slot-value mat '%shader)
                     (let ((shader (make-instance 'rl-shader)))
                       (setf (c-struct shader)
                             (material.shader c-elt))
                       shader)

                     (slot-value mat '%maps)
                     (let ((maps (make-instance 'rl-material-maps)))
                       (setf (slot-value maps '%cl-array)
                             (make-rl-*-array
                              (autowrap:c-aref (material.maps c-elt) 0 'claylib/wrap:material-map)
                              11)) ; TODO 11 entries in MaterialMapIndex, is this the correct num?
                       maps))
            collect mat)))
    (make-array num
                :element-type 'rl-material
                :initial-contents contents)))

(defmethod (setf sequences:elt) (value (sequence rl-materials) index)
  (check-type value rl-material)
  (cffi:foreign-funcall "memcpy"
                        :pointer (autowrap:ptr (c-struct (elt sequence index)))
                        :pointer (autowrap:ptr (c-struct value))
                        :int +foreign-material-size+
                        :void))
