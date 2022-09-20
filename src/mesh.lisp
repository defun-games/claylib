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



(defclass rl-meshes (sequences:sequence)
  ((%c-struct :initarg :c-struct
              :initform (error "Must give initial :C-STRUCT argument.")
              :type 'claylib/wrap:mesh
              :accessor c-struct
              :documentation "The wrapper for the first Mesh in a meshes array.")
   (%mesh-count :initarg :mesh-count
                :initform (error "Must give initial :MESH-COUNT argument.")
                :type integer
                :accessor mesh-count
                :documentation "The number of meshes in this raylib array. This slot must be
synchronized with the length of the array.")))

(defconstant +foreign-mesh-size+ (cffi:foreign-type-size '(:struct mesh)))

(defun copy-mesh (from-ptr to-ptr)
  "Copy the mesh wrapper data at FROM-PTR to TO-PTR."
  (cffi:foreign-funcall "memcpy"
                        :pointer to-ptr
                        :pointer from-ptr
                        :int +foreign-mesh-size+
                        :void))

(defmethod sequences:length ((sequence rl-meshes))
  (mesh-count sequence))

(defmethod sequences:elt ((sequence rl-meshes) index)
  (check-type index integer)
  (when (>= index (mesh-count sequence))
    (error "Index out of bounds."))
  ;; TODO: to be consistent, this should return an rl-mesh, but that requires allocating a new
  ;; rl-mesh upon each access. Hmm...
  (autowrap:c-aref sequence index 'claylib/wrap:mesh))

(defmethod (setf sequences:elt) (value (sequence rl-meshes) index)
  (check-type index integer)
  (let ((value (if (typep value 'rl-mesh) (c-struct value) value)))
    (copy-mesh (autowrap:c-aptr sequence index 'claylib/wrap:mesh)
               (autowrap:ptr (c-struct value)))))

(defmethod sequences:adjust-sequence ((sequence rl-meshes) length
                                      &key initial-contents initial-element)
  "Adjust the length of the given rl-meshes sequence. The length may only be decreased.

Or, if either INITIAL-ELEMENT (an rl-mesh) or INITIAL-CONTENTS (a list of rl-mesh objects) is given,
return a newly allocated sequence with the initial element or contents."
  (cond
    ((and initial-element initial-contents)
     (error "Cannot specify both :INITIAL-ELEMENT and :INITIAL-CONTENTS"))

    (initial-element
     (let ((new-meshes (make-instance 'rl-meshes
                                      :mesh-count length
                                      :c-struct (autowrap:alloc 'claylib/wrap:mesh length))))
       (loop for i below length
             do (copy-mesh (autowrap:c-aptr (c-struct new-meshes) i 'claylib/wrap:mesh)
                           (autowrap:ptr (c-struct initial-element)))
             finally return new-meshes)))

    (initial-contents
     (let ((new-meshes (make-instance 'rl-meshes
                                      :mesh-count length
                                      :c-struct (autowrap:alloc 'claylib/wrap:mesh length))))
       (loop for i below length
             for mesh in initial-contents
             do (copy-mesh (autowrap:c-aptr (c-struct new-meshes) i 'claylib/wrap:mesh)
                           (autowrap:ptr (c-struct mesh)))
             finally return new-meshes)))

    (t
     ;; Free the memory beyond the adjusted length index
     ;; TODO will this cause double free later?
     (loop for i from length to (1- (mesh-count sequence))
           do (free (autowrap:c-aref (c-struct sequence) i 'claylib/wrap:mesh))
           finally (setf (mesh-count sequence) length)
           return sequence))))

(defmethod sequences:make-sequence-like ((sequence rl-meshes) length
                                         &key initial-contents initial-element)
  (let ((new-meshes (make-instance 'rl-meshes
                                   :mesh-count length
                                   :c-struct (autowrap:alloc new-seq 'mesh length))))
    (cond
      ((and initial-element initial-contents)
       (error "Cannot specify both :INITIAL-ELEMENT and :INITIAL-CONTENTS"))

      (initial-element
       (loop for i below length
             do (copy-mesh (autowrap:c-aptr (c-struct new-meshes) i 'claylib/wrap:mesh)
                           (autowrap:ptr (c-struct initial-element)))))

      (initial-contents
       (loop for i below length
             for mesh in initial-contents
             do (copy-mesh (autowrap:c-aptr (c-struct new-meshes) i 'claylib/wrap:mesh)
                           (autowrap:ptr (c-struct mesh)))))

      (t
       (loop for i below length
             do (copy-mesh (autowrap:c-aptr (c-struct new-meshes) i 'claylib/wrap:mesh)
                           (autowrap:c-aptr (c-struct sequence) i 'claylib/wrap:mesh)))))
    new-meshes))
