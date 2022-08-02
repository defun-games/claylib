(in-package #:claylib)

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
    :accessor c-struct)))

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



(defclass model (3d-object)
  ((%asset :initarg :asset
           :type model-asset
           :accessor asset)
   (%scale :initarg :scale
           :type rl-vector3
           :accessor scale)
   (%tint :initarg :tint
          :type rl-color
          :accessor tint)))

(defreader c-asset model c-asset asset)
(definitializer model (scale rl-vector3 nil) (tint rl-color nil))

(default-slot-value model %scale (make-vector3 1 1 1))
(default-slot-value model %tint +white+)
(default-slot-value model %rot-angle 0.0)
(default-slot-value model %rot-axis (make-vector3 0 1 0))

(defun make-model (model-asset x y z
                   &rest args &key scale tint rot-angle rot-axis)
  "Make a model ready for drawing. Loads MODEL-ASSET when not already loaded."
  (declare (ignore scale tint rot-angle rot-axis))
  (load-asset model-asset)
  (apply #'make-instance 'model
         :asset model-asset
         :pos (make-vector3 x y z)
         args))

(defmethod free ((obj model))
  (free (scale obj))
  (call-next-method))

(defmethod draw-object ((obj model))
  (claylib/ll:draw-model-ex (c-asset obj)
                            (c-struct (pos obj))
                            (c-struct (rot-axis obj))
                            (rot-angle obj)
                            (c-struct (scale obj))
                            (c-struct (tint obj))))
