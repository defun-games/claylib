(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-shader ()
    ((%c-struct
      :type claylib/ll:shader
      :initform (autowrap:alloc 'claylib/ll:shader)
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
      :initform (autowrap:alloc 'claylib/ll:material-map)
      :accessor c-struct))))

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

(default-free rl-material-map)
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
      :initform (autowrap:alloc 'claylib/ll:material)
      :accessor c-struct))))

(defcreader params rl-material params material)  ; TODO: Array

(defcwriter params rl-material params material number float)  ; TODO: Array
(defcwriter-struct shader rl-material shader material shader
  id locs)
(defcwriter-struct maps rl-material maps material material-map ; Array/pointer
  texture color value)

(definitializer rl-material
  :struct-slots ((%shader) (%maps))
  :pt-accessors ((params number float)))

(default-free rl-material)
(default-free-c claylib/ll:material unload-material)
