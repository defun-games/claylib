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

(defun gen-mesh-cylinder (radius height slices
                          &key (mesh (make-instance 'rl-mesh)))
  "Generate a mesh cylinder. Allocates a new RL-MESH unless you pass one."
  (check-type radius number)
  (check-type height number)
  (check-type slices integer)
  (check-type mesh rl-mesh)
  (claylib/ll:gen-mesh-cylinder (c-struct mesh)
                                (coerce radius 'float)
                                (coerce height 'float)
                                slices)
  mesh)

(defun gen-mesh-cube (width height length
                      &key (mesh (make-instance 'rl-mesh)))
  "Generate a mesh rectangular prism, a.k.a. 'cube.' Allocates a new RL-MESH unless you pass one."
  (check-type width number)
  (check-type height number)
  (check-type length number)
  (claylib/ll:gen-mesh-cube (c-struct mesh)
                            (coerce width 'float)
                            (coerce height 'float)
                            (coerce length 'float))
  mesh)
