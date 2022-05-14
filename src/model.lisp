(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-model ()
    ((%transform :initarg :transform
                 :type rl-matrix
                 :reader transform)
     (%meshes :initarg :meshes
                                        ; TODO: type (pointer)
              :reader meshes)
     (%materials :initarg :materials
                                        ; TODO: type (pointer)
                 :reader materials)
     (%bones :initarg :bones
                                        ; TODO: type (pointer)
             :reader bones)
     (%bind-pose :initarg :bind-pose
                 :type rl-transform ; pointer
                 :reader bind-pose)
     (%c-struct
      :type claylib/ll:model
      :initform (autowrap:alloc 'claylib/ll:model)
      :accessor c-struct))))

(defcreader mesh-count rl-model mesh-count model)
(defcreader material-count rl-model material-count model)
(defcreader mesh-material rl-model mesh-material model) ; pointer
(defcreader bone-count rl-model bone-count model)

(defcwriter mesh-count rl-model mesh-count model integer)
(defcwriter material-count rl-model material-count model integer)
(defcwriter mesh-material rl-model mesh-material model integer) ; pointer
(defcwriter bone-count rl-model bone-count model integer)
(defcwriter-struct transform rl-model transform model matrix
  m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15)
(defcwriter-struct meshes rl-model meshes model mesh ; pointer
  vertex-count triangle-count vertices texcoords texcoords2 normals tangents colors
  indices anim-vertices anim-normals bone-ids bone-weights vao-id vbo-id)
(defcwriter-struct materials rl-model materials model material ; pointer
  shader maps params)
(defcwriter-struct bones rl-model bones model bone-info ; pointer
  name parent)
(defcwriter-struct bind-pose rl-model bind-pose model transform ; pointer
  trans rot scale)

(definitializer rl-model
  (transform rl-matrix) (mesh-count integer) (material-count integer) (meshes rl-mesh)
  (materials rl-material) (mesh-material integer) (bone-count integer)
  (bones rl-bone-info) (bind-pose rl-transform))

(default-free rl-model)
(default-free-c claylib/ll:model unload-model t)

(defun-pt load-model-from-mesh claylib/ll:load-model-from-mesh
  "Load a model from a passed-in mesh. Allocates a new RL-MODEL unless you pass one."
  (model rl-model nil (make-instance 'rl-model))
  (mesh rl-mesh))



(defclass model (rl-model 3d-object)
  ((%scale :initarg :scale
           :type rl-vector3
           :accessor scale)
   (%tint :initarg :tint
          :type rl-color
          :accessor tint)))

(definitializer model (scale rl-vector3 nil) (tint rl-color nil))

(defmethod free ((obj model))
  (free (scale obj))
  (call-next-method))

(defmethod draw-object ((obj model))
  (claylib/ll:draw-model-ex (c-struct obj)
                            (c-struct (pos obj))
                            (c-struct (rot-axis obj))
                            (rot-angle obj)
                            (c-struct (scale obj))
                            (c-struct (tint obj))))
