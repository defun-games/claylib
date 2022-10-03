(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-mesh ()
    ((%c-struct
      :type claylib/ll:mesh
      :initform (autowrap:calloc 'claylib/ll:mesh)
      :accessor c-struct))))

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

(default-free rl-mesh)
(default-free-c claylib/ll:mesh unload-mesh t)



;; TODO: Does autowrap give me something like this already? I need to know the size for memcpy later
(cffi:defcstruct mesh
  (vertex-count :int)
  (triangle-count :int)
  (vertices :pointer)
  (tex-coords :pointer)
  (tex-coords2 :pointer)
  (normals :pointer)
  (tangents :pointer)
  (colors :pointer)
  (indices :pointer)
  (anim-vertices :pointer)
  (anim-normals :pointer)
  (bone-ids :pointer)
  (bone-weights :pointer)
  (vao-id :uint)
  (vbo-id :pointer))
(defconstant +foreign-mesh-size+ (cffi:foreign-type-size '(:struct mesh)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-meshes (sequences:sequence)
    ((%cl-array :type (array rl-mesh 1)
                :initarg :cl-array
                :reader cl-array
                :documentation "An RL-MESH array tracking the C Mesh array underneath."))))

(defun make-meshes-array (c-struct mesh-count)
  "Make an array of rl-mesh objects using MESH-COUNT elements of the Mesh wrapper C-STRUCT.

Warning: this can refer to bogus C data if MESH-COUNT does not match the real C array length."
  (let ((contents (loop for i below mesh-count
                        for mesh = (make-instance 'rl-mesh)
                        do (setf (slot-value mesh '%c-struct)
                                 (autowrap:c-aref c-struct i 'claylib/wrap:mesh))
                        collect mesh)))
    (make-array mesh-count
                :element-type 'rl-mesh
                :initial-contents contents)))

(defmethod sequences:length ((sequence rl-meshes))
  (length (cl-array sequence)))

(defmethod sequences:elt ((sequence rl-meshes) index)
  (elt (cl-array sequence) index))

(defmethod (setf sequences:elt) (value (sequence rl-meshes) index)
  "Set the element at INDEX of the rl-meshes SEQUENCE such that it contains a copy of the C data in
VALUE (an rl-mesh).

Since rl-meshes deals with a C array (contiguous memory), we cannot simply change the data of any
particular element by changing the pointer, we must memcpy it in.

Example:
(set-meshes-element n rl-meshes rl-mesh)

\"To set the nth element of rl-meshes, set the nth element of its %array slot & overwrite the
nth element of the underlying c-struct.\""
  (cffi:foreign-funcall "memcpy"
                        :pointer (autowrap:ptr (c-struct (elt sequence index)))
                        :pointer (autowrap:ptr (c-struct value))
                        :int +foreign-mesh-size+
                        :void))

;; TODO: Necessary? Can we safely leave this unimplemented?
;; (defmethod sequences:adjust-sequence ((sequence rl-meshes) length
;;                                       &key initial-contents initial-element)
;; ;; Cannot meaningfully change length
;;   (unless (= length (length rl-meshes))
;;     (error "Cannot change the length of a C array."))
;;   (cond
;;     ((and initial-contents initial-element)
;;      (error "Cannot give both INITIAL-CONTENTS and INITIAL-ELEMENT"))

;;     (initial-contents
;;      (loop for rl-mesh in initial-contents
;;            do ; copy each mesh in
;;            ))

;;     (initial-element
;;      )))

;; TODO: Necessary? Can we safely leave this unimplemented?
;; (defmethod sequences:make-sequence-like ((sequence rl-meshes) length
;;                                          &key initial-contents initial-ellement))
