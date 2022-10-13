(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-shader ()
    ((%c-struct
      :type claylib/ll:shader
      :initform (autowrap:calloc 'claylib/ll:shader)
      :accessor c-struct))))

(defcreader id rl-shader id shader)
(defcreader locs rl-shader locs shader)  ; TODO: Array/pointer

(defcwriter id rl-shader id shader integer)
(defcwriter locs rl-shader locs shader integer)  ; TODO: Array/pointer

(definitializer rl-shader
  :pt-accessors ((id integer)
                 (locs integer)))

(default-free rl-shader)
(default-free-c claylib/ll:shader unload-shader)



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
      :initform (autowrap:calloc 'claylib/ll:material-map)
      :accessor c-struct))))

(defcreader value rl-material-map value material-map)

(defcwriter value rl-material-map value material-map number float)
(defcwriter-struct texture rl-material-map texture material-map texture
  id width height mipmaps data-format)
(defcwriter-struct color rl-material-map color material-map color
  r g b a)

(defmethod sync-children ((obj rl-material-map))
  (unless (eq (c-struct (texture obj))
              (material-map.texture (c-struct obj)))
    (free-later (c-struct (texture obj)))
    (setf (c-struct (texture obj))
          (material-map.texture (c-struct obj))))
  (unless (eq (c-struct (color obj))
              (material-map.color (c-struct obj)))
    (free-later (c-struct (color obj)))
    (setf (c-struct (color obj))
          (material-map.color (c-struct obj)))))

(definitializer rl-material-map
  :struct-slots ((%texture)
                 (%color color))
  :pt-accessors ((value number float)))

(default-free rl-material-map %texture %color)
(default-free-c claylib/ll:material-map)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-material ()
    ((%shader :initarg :shader
              :type rl-shader
              :reader shader)
     (%maps :initarg :maps
            :type rl-material-map  ; TODO: Array/pointer
            :reader maps)
     (%c-struct
      :type claylib/ll:material
      :initform (autowrap:calloc 'claylib/ll:material)
      :accessor c-struct))))

(defcreader params rl-material params material)  ; TODO: Array

(defcwriter params rl-material params material number float)  ; TODO: Array
(defcwriter-struct shader rl-material shader material shader
  id locs)
(defcwriter-struct maps rl-material maps material material-map ; Array/pointer
  texture color value)

(defmethod sync-children ((obj rl-material))
  (flet ((i0 (array type)
           (autowrap:c-aref array 0 type)))
    (unless (eq (c-struct (shader obj))
                (material.shader (c-struct obj)))
      (free-later (c-struct (shader obj)))
      (setf (c-struct (shader obj))
            (material.shader (c-struct obj))))
    (unless (cffi-sys:null-pointer-p
             (autowrap:ptr (c-struct obj)))
      (when (and (data-valid-p (c-struct obj))
                 (array-valid-p (material.maps (c-struct obj))
                                12
                                'claylib/ll:material-map))
        (unless (eq (c-struct (maps obj))
                    (i0 (material.maps (c-struct obj)) 'claylib/ll:material-map))
          (free-later (c-struct (maps obj)))
          (setf (c-struct (maps obj))
                (i0 (material.maps (c-struct obj)) 'claylib/ll:material-map))))))
  (sync-children (maps obj)))

(definitializer rl-material
  :struct-slots ((%shader) (%maps))
  :pt-accessors ((params number float)))

(default-free rl-material %shader %maps)
(default-free-c claylib/ll:material unload-material)



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
  (defclass rl-material-maps (sequences:sequence)
    ((%cl-array :type (array rl-material-map 1)
                :initarg :cl-array
                :reader cl-array
                :documentation "An RL-MATERIAL-MAP array tracking the C MaterialMap array underneath."))))

(defun make-material-map-array (c-struct &optional (map-count 11)) ; 11 entries in MaterialMapIndex
  (let ((contents (loop for i below map-count
                        for map = (make-instance 'rl-material-map)
                        for c-elt = (autowrap:c-aref c-struct i 'claylib/wrap:material-map)
                        do (setf (slot-value map '%c-struct)
                                 c-elt

                                 (slot-value map '%texture)
                                 (let ((tex (make-instance 'rl-texture)))
                                   (setf (c-struct tex) (material-map.texture c-elt))
                                   tex)

                                 (slot-value map '%color)
                                 (let ((col (make-instance 'rl-color)))
                                   (setf (c-struct col) (material-map.color c-elt))
                                   col))
                        collect map)))
    (make-array map-count
                :element-type 'rl-material-map
                :initial-contents contents)))

(defmethod sequences:length ((sequence rl-material-maps))
  (length (cl-array sequence)))

(defmethod sequences:elt ((sequence rl-material-maps) index)
  (elt (cl-array sequence) index))

(defmethod (setf sequences:elt) (value (sequence rl-material-maps) index)
  (check-type value rl-material-map)
  (cffi:foreign-funcall "memcpy"
                        :pointer (autowrap:ptr (c-struct (elt sequence index)))
                        :pointer (autowrap:ptr (c-struct value))
                        :int +foreign-material-map-size+
                        :void))



(cffi:defcstruct shader
  (id :uint)
  (locs :pointer))
(cffi:defcstruct material
  (shader (:struct shader))
  (maps :pointer)
  (params :float :count 4))
(defconstant +foreign-material-size+ (cffi:foreign-type-size '(:struct material)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-materials (sequences:sequence)
    ((%cl-array :type (array rl-material 1)
                :initarg :cl-array
                :reader cl-array
                :documentation "An RL-MATERIAL array tracking the C Material array underneath."))))

(defun make-material-array (c-struct material-count)
  "Make an array of rl-material objects using MATERIAL-COUNT elements of the Material wrapper
C-STRUCT.

Warning: this can refer to bogus C data if MATERIAL-COUNT does not match the real C array length."
  (let ((contents (loop for i below material-count
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
                                         (make-material-map-array (material.maps c-elt)))
                                   maps))
                        collect mat)))
    (make-array material-count
                :element-type 'rl-material
                :initial-contents contents)))

(defmethod sequences:length ((sequence rl-materials))
  (length (cl-array sequence)))

(defmethod sequences:elt ((sequence rl-materials) index)
  (elt (cl-array sequence) index))

(defmethod (setf sequences:elt) (value (sequence rl-materials) index)
  (check-type value rl-material)
  (cffi:foreign-funcall "memcpy"
                        :pointer (autowrap:ptr (c-struct (elt sequence index)))
                        :pointer (autowrap:ptr (c-struct value))
                        :int +foreign-material-size+
                        :void))
