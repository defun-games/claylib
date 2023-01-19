(in-package #:claylib)

(default-unload claylib/ll:mesh unload-mesh t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-mesh ()
    ((%c-struct
      :type claylib/ll:mesh
      :accessor c-struct))
    (:default-initargs
     :c-struct (autowrap:calloc 'claylib/ll:mesh))))

(defcreader vertex-count rl-mesh vertex-count mesh)
(defcreader triangle-count rl-mesh triangle-count mesh)
;; TODO: All of the below fields except vao-id/vbo-id are array/pointers
(defcreader vertices rl-mesh vertices mesh)
(defcreader texcoords rl-mesh texcoords mesh)
(defcreader texcoords2 rl-mesh texcoords2 mesh)
(defcreader normals rl-mesh normals mesh)
(defcreader tangents rl-mesh tangents mesh)
(defcreader colors rl-mesh colors mesh)
(defcreader indices rl-mesh indices mesh)
(defcreader anim-vertices rl-mesh anim-vertices mesh)
(defcreader anim-normals rl-mesh anim-normals mesh)
(defcreader bone-ids rl-mesh bone-ids mesh)
(defcreader bone-weights rl-mesh bone-weights mesh)
(defcreader vao-id rl-mesh vao-id mesh)
(defcreader vbo-id rl-mesh vbo-id mesh)  ; pointer

(define-print-object rl-mesh
  (vertex-count triangle-count vertices texcoords texcoords2 normals tangents colors indices anim-vertices anim-normals bone-ids bone-weights vao-id vbo-id))

(defcwriter vertex-count rl-mesh vertex-count mesh integer)
(defcwriter triangle-count rl-mesh triangle-count mesh integer)
;; TODO: All of the below fields except vao-id/vbo-id are array/pointers
(defcwriter vertices rl-mesh vertices mesh number float)
(defcwriter texcoords rl-mesh texcoords mesh number float)
(defcwriter texcoords2 rl-mesh texcoords2 mesh number float)
(defcwriter normals rl-mesh normals mesh number float)
(defcwriter tangents rl-mesh tangents mesh number float)
(defcwriter colors rl-mesh colors mesh integer)
(defcwriter indices rl-mesh indices mesh integer)
(defcwriter anim-vertices rl-mesh anim-vertices mesh number float)
(defcwriter anim-normals rl-mesh anim-normals mesh number float)
(defcwriter bone-ids rl-mesh bone-ids mesh integer)
(defcwriter bone-weights rl-mesh bone-weights mesh number float)
(defcwriter vao-id rl-mesh vao-id mesh integer)
(defcwriter vbo-id rl-mesh vbo-id mesh integer)  ; pointer

(definitializer rl-mesh
  :pt-accessors ((vertex-count integer)
                 (triangle-count integer)
                 (vertices number float)
                 (texcoords number float)
                 (texcoords2 number float)
                 (normals number float)
                 (tangents number float)
                 (colors integer)
                 (indices integer)
                 (anim-vertices number float)
                 (anim-normals number float)
                 (bone-ids integer)
                 (bone-weights number float)
                 (vao-id integer)
                 (vbo-id integer)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass mesh (rl-mesh) ()))

(define-print-object mesh
    ())


(defconstant +foreign-mesh-size+ (autowrap:sizeof 'claylib/ll:mesh))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-meshes (rl-sequence)
    ((%cl-array :type (array rl-mesh 1)))))

(define-print-object rl-meshes
    ())

(defmethod make-rl-*-array ((c-struct claylib/wrap:mesh) num)
  (let ((contents (loop for i below num
                        for mesh = (make-instance 'rl-mesh)
                        do (setf (slot-value mesh '%c-struct)
                                 (autowrap:c-aref c-struct i 'claylib/wrap:mesh))
                        collect mesh)))
    (make-array num
                :element-type 'rl-mesh
                :initial-contents contents)))

(defmethod (setf sequences:elt) (value (sequence rl-meshes) index)
  "Set the element at INDEX of the rl-meshes SEQUENCE such that it contains a copy of the C data in
VALUE (an rl-mesh).

Since rl-meshes deals with a C array (contiguous memory), we cannot simply change the data of any
particular element by changing the pointer, we must memcpy it in.

Example:
(set-meshes-element n rl-meshes rl-mesh)

\"To set the nth element of rl-meshes, set the nth element of its %cl-array slot & overwrite the
nth element of the underlying c-struct.\""
  (check-type value rl-mesh)
  (cffi:foreign-funcall "memcpy"
                        :pointer (autowrap:ptr (c-struct (elt sequence index)))
                        :pointer (autowrap:ptr (c-struct value))
                        :int +foreign-mesh-size+
                        :void))
