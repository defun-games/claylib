(in-package #:claylib)

(defclass rl-mesh ()
  ((%c-struct
    :type claylib/ll:mesh
    :initform (autowrap:alloc 'claylib/ll:mesh)
    :accessor c-struct)))

(defcreader vertex-count rl-mesh vertex-count mesh)
(defcreader triangle-count rl-mesh triangle-count mesh)
;; All of the below fields except vao-id are pointers
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
(defcreader vbo-id rl-mesh vbo-id mesh)

(defcwriter vertex-count rl-mesh vertex-count mesh integer)
(defcwriter triangle-count rl-mesh triangle-count mesh integer)
;; All of the below fields except vao-id are pointers
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
(defcwriter vbo-id rl-mesh vbo-id mesh integer)

(definitializer rl-mesh
  (vertex-count integer) (triangle-count integer) (vertices number float)
  (texcoords number float) (texcoords2 number float) (normals number float)
  (tangents number float) (colors integer) (indices integer) (anim-vertices number float)
  (anim-normals number float) (bone-ids integer) (bone-weights number float)
  (vao-id integer) (vbo-id integer))

(default-free rl-mesh)
(default-free-c claylib/ll:mesh unload-mesh)



;; TODO: inherit from sequence, define sequence methods to ease access
(defclass rl-meshes ()
  ((%array :type simple-vector ;; TODO define array type to ensure elements are rl-mesh objects
           :accessor array
           :documentation "A Lisp array of RL-MESH objects that tracks the C Mesh array
(i.e. %C-STRUCT) underneath.")
   ;; TODO: get rid of %c-struct, it is unnecessary after initializing the array
   (%c-struct :initarg :c-struct
              :initform (error "Must give initial :C-STRUCT argument.")
              :type 'claylib/wrap:mesh
              :reader c-struct
              :documentation "The wrapper for the first Mesh in a meshes array.")))

(defmethod initialize-instance :after ((meshes rl-meshes) &key c-struct mesh-count)
  ;; Note: C-STRUCT is the mesh wrapper at the start of the array.
  (if c-struct
      (check-type c-struct claylib/wrap:mesh)
      (error "Must provide c-struct in order to initialize the Lisp array."))
  (if mesh-count
      (check-type mesh-count integer)
      (error "Must provide mesh-count in order to read the correct amount of Raylib Meshes."))
  (let ((contents (loop for i below mesh-count
                        for mesh = (make-instance 'rl-mesh)
                        do (setf (slot-value mesh '%c-struct)
                                 (autowrap:c-aref c-struct i 'claylib/wrap:mesh))
                        collect mesh)))
    (setf (array meshes)
          (make-array mesh-count
                      :element-type 'rl-mesh
                      :initial-contents contents))))

;; TODO: Does autowrap give me something like this already? I need to know the size for memcpy later
;; but this method is not guaranteed to work as many of these values are pointers which are not
;; guaranteed to have the same size as ints.
(cffi:defcstruct mesh
  (vertex-count :int)
  (triangle-count :int)
  (vertices :int)
  (tex-coords :int)
  (tex-coords2 :int)
  (normals :int)
  (tangents :int)
  (colors :int)
  (indices :int)
  (anim-vertices :int)
  (anim-normals :int)
  (bone-ids :int)
  (bone-weights :int)
  (vao-id :uint)
  (vbo-id :int))
(defconstant +foreign-mesh-size+ (cffi:foreign-type-size '(:struct mesh)))

;; TODO:
;; - Update the parent's mesh-count. Is this done with sync-children?
;; - Make this into a setf on rl-meshes if possible.
;;   A bad approach: define a setf that takes a new rl-meshes (possible just a modified version of
;;   the original) and compares each element. When they differ, memcpy the new element onto the old.
(defun set-meshes-elt (n rl-meshes rl-mesh)
  "Set the Nth element of RL-MESHES to RL-MESH.

Example:
(set-meshes-element n rl-meshes rl-mesh)

\"To set the nth element of rl-meshes, set the nth element of its %array slot & overwrite the
nth element of the underlying c-struct.\""
  ;; When memcpying, this setf is unnecessary. The lisp array will contain the new rl-mesh object
  ;; but the C array will already have that data copied into it.
  ;;
  ;; What I should do instead is track only the lisp array because it gives me index-wise access
  ;; into the C data anyway (each rl-mesh has a c-struct which is a wrapper around each Mesh in the
  ;; C array). Then to set an rl-mesh in an rl-meshes, it's a matter of memcpying the new data to
  ;; the known location
  ;;
  ;; (setf (elt (array rl-meshes) n) rl-mesh)
  (cffi:foreign-funcall "memcpy"
                        ;; :pointer (autowrap:c-aptr (c-struct rl-meshes) n 'claylib/wrap:mesh)
                        :pointer (autowrap:ptr (c-struct (elt (array rl-meshes) n)))
                        :pointer (autowrap:ptr (c-struct rl-mesh))
                        :int +foreign-mesh-size+
                        :void))
