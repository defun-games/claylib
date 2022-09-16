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



(defclass rl-meshes (sequence)
  ((%pointer :initarg :pointer
             :initform (error "Must give initial :POINTER argument.")
             :accessor pointer
             :documentation "The pointer a raylib Model keeps in their meshes field.")
   ;; Note: On init, we can set to the mesh-count of the model. But we must update mesh-count
   ;; manually whenever the # of elements changes
   (%mesh-count :initarg :mesh-count
                :initform (error "Must give initial :MESH-COUNT argument.")
                :reader mesh-count
                :documentation "The number of meshes in this raylib array.")))

(defmethod sequences:length ((sequence rl-meshes))
  (mesh-count sequence))

(defmethod sequences:elt ((sequence rl-meshes) index)
  (check-type index integer)
  (when (>= index (mesh-count sequence))
    (error "Index out of bounds."))
  (autowrap:c-aref sequence index 'mesh))

(defmethod (setf sequences:elt) (value (sequence rl-meshes) index)
  (check-type index integer)
  ())

(defmethod sequences:adjust-sequence ((sequence rl-meshes) length
                                      &key initial-contents initial-element)
  )

(defmethod sequences:make-sequence-like ((sequence rl-meshes) length
                                         &key initial-contents initial-element))
